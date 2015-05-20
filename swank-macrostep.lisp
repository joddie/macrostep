
(defpackage swank-macrostep
  (:use cl swank)
  (:import-from swank
                #:with-buffer-syntax
                #:to-string
                #:macroexpand-all
                #:compiler-macroexpand-1)
  (:export #:macrostep-expand-1
           #:macro-form-p))

(in-package #:swank-macrostep)

(defun macrostep-expand-1 (string binding-strings &optional compiler-macros?)
  (with-buffer-syntax ()
    (let* ((form (read-from-string string))
           (bindings (mapcar #'read-from-string binding-strings))
           (env (compute-environment bindings))
           (expansion
             (multiple-value-bind (expansion expanded?)
                 (macroexpand-1 form env)
               (if expanded?
                   expansion
                   (if (not compiler-macros?)
                       (error "Not a macro form.")
                       (multiple-value-bind (expansion expanded?)
                           (compiler-macroexpand-1 form env)
                         (if expanded?
                             expansion
                             (error "Not a macro or compiler-macro form.")))))))
           (pretty-expansion (to-string expansion)))
      (list pretty-expansion
            (multiple-value-bind (expansion* tracking-stream)
                (tracking-read-from-string pretty-expansion)
              (multiple-value-bind (macros compiler-macros)
                  (collect-macro-forms expansion*)
                (flet ((collect-positions (forms type)
                         (loop for form in forms
                               for bounds = (cdr (assoc form (forms-of tracking-stream)))
                               when bounds
                                 collect (destructuring-bind (start end)
                                             bounds
                                           ;; this assumes that the operator
                                           ;; starts right next to the opening
                                           ;; parenthesis. I guess we could be
                                           ;; more forgiving with some
                                           ;; cleverness on the Emacs side.
                                           (let ((op-end (+ start (length (to-string (first form))))))
                                             (list type
                                                   start (position-line start tracking-stream)
                                                   op-end (position-line op-end tracking-stream)
                                                   end (position-line end tracking-stream)))))))
                  (append (collect-positions macros :macro)
                          (collect-positions compiler-macros :compiler-macro)))))))))

(defun macro-form-p (string binding-strings &optional compiler-macros?)
  (with-buffer-syntax ()
    (let* ((form (read-from-string string))
           (bindings (mapcar #'read-from-string binding-strings))
           (env (compute-environment bindings)))
      (macro-form-type form env compiler-macros?))))

(defun macro-form-type (sexp environment compiler-macros?)
  (cond
    ((or (not (consp sexp))
         (not (symbolp (car sexp))))
     nil)
    ((macro-function (car sexp) environment)
     :macro)
    ((and compiler-macros?
          (compiler-macro-function (car sexp) environment))
     :compiler-macro)
    (t
     nil)))

(defun compute-environment (binding-forms)
  (eval (make-environment-extractor
         (mapcar #'read-from-string binding-forms))))

(defun make-environment-extractor (binding-lists)
  (if (null binding-lists)
      (let ((return-env (gensym)))
        `(macrolet ((,return-env (&environment env) env))
           (,return-env)))
    (let ((binding-list (car binding-lists))
          (subexpression (make-environment-extractor (cdr binding-lists))))
      `(macrolet ,binding-list ,subexpression))))

#-sbcl
(defun collect-macro-forms (form &optional environment)
  (let ((real-macroexpand-hook *macroexpand-hook*))
    (macrolet ((make-form-collector (variable)
                 `(lambda (macro-function form environment)
                    (setq ,variable (cons form ,variable))
                    (funcall real-macroexpand-hook
                             macro-function form environment))))
      (let* ((macro-forms '())
             (compiler-macro-forms '())
             (*macroexpand-hook* (make-form-collector macro-forms))
             (expansion (ignore-errors (macroexpand-all form)))
             (*macroexpand-hook* (make-form-collector compiler-macro-forms)))
        (handler-bind ((warning #'muffle-warning))
          (ignore-errors
            (compile nil `(lambda () ,expansion))))
        (values macro-forms compiler-macro-forms)))))

#+sbcl
(defun collect-macro-forms (form &optional environment)
  (let ((macro-forms '())
        (compiler-macro-forms '()))
    (sb-walker:walk-form
     form environment
     (lambda (form context environment)
       (declare (ignore context))
       (when (and (consp form)
                  (symbolp (car form)))
         (cond ((macro-function (car form) environment)
                (push form macro-forms))
               ((compiler-macro-function (car form) environment)
                (push form compiler-macro-forms))))
       form))
    (values macro-forms compiler-macro-forms)))

;;;; FORM-TRACKING-STREAM

(defclass form-tracking-stream (swank/gray:fundamental-character-input-stream)
  (;; The underlying stream.
   (source :initarg :source :accessor source-of)
   (position :initform 0 :accessor position-of)
   ;; Track the position of each #\Newline that occurred so that, if
   ;; desired, a line/column can be calculated for any position.
   (newlines :initform (make-array 10 :adjustable t :fill-pointer 0)
             :accessor newlines-of)
   (forms :initform nil :accessor forms-of)))

(defmethod swank/gray:stream-read-char ((stream form-tracking-stream))
  (handler-case
      (let ((pos (position-of stream))
            (result (read-char (source-of stream))))
        (incf (position-of stream))
        (when (eql result #\Newline)
          (let* ((newlines (newlines-of stream))
                 (n (length newlines)))
            (when (or (zerop n) (> pos (aref newlines (1- n))))
              (vector-push-extend pos newlines))))
        result)
    (end-of-file () :eof)))

(defmethod swank/gray:stream-unread-char ((stream form-tracking-stream) character)
  (prog1 (unread-char character (source-of stream))
    (decf (position-of stream))))

(defun annotate-position (stream object start)
  (push (list object start (position-of stream))
        (forms-of stream)))

(defun form-tracking-stream-p (stream)
  (typep stream 'form-tracking-stream))

(defun position-line (position tracking-stream)
  (or (position-if (lambda (newline-pos)
                     (> newline-pos position))
                   (newlines-of tracking-stream))
      (length (newlines-of tracking-stream))))

(defun tracking-read-from-string (string &key (readtable *readtable*))
  (with-input-from-string (string-stream string)
    (let ((instrumented-readtable (copy-readtable readtable))
          (tracking-stream (make-instance 'form-tracking-stream
                                          :source string-stream)))
      ;; we could do this for every readtable char, using
      ;; named-readtables::do-readtable, but for our purposes here,
      ;; #\( suffices.
      (multiple-value-bind (fn non-terminating-p)
          (get-macro-character #\( readtable)
        (set-macro-character #\(
                             (lambda (&rest args)
                               (let ((start (1- (position-of tracking-stream)))
                                     (object (apply fn args)))
                                 (annotate-position tracking-stream object start)
                                 object))
                             non-terminating-p
                             instrumented-readtable))
      (let ((*readtable* instrumented-readtable))
        (values (read tracking-stream) tracking-stream)))))

(provide :swank-macrostep)
