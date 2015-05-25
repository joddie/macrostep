
(defpackage swank-macrostep
  (:use cl swank)
  (:import-from swank
		#:*macroexpand-printer-bindings*
                #:with-buffer-syntax
		#:with-bindings
                #:to-string
                #:macroexpand-all
                #:compiler-macroexpand-1)
  (:export #:macrostep-expand-1
           #:macro-form-p))

(in-package #:swank-macrostep)

(defun pprint-to-string (object &optional pprint-dispatch)
  (let ((*print-pprint-dispatch* (or pprint-dispatch *print-pprint-dispatch*)))
    (with-bindings *macroexpand-printer-bindings*
      (to-string object))))

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
           (pretty-expansion (pprint-to-string expansion)))
      (multiple-value-bind (macros compiler-macros)
          (collect-macro-forms expansion)
        (let* ((all-macros (append macros compiler-macros))
               (positions (collect-form-positions expansion
                                                  pretty-expansion
                                                  all-macros)))
          (list pretty-expansion
                (loop for form in all-macros
                      for (start end) in positions
                      when (and start end)
                        collect (let ((op-name (to-string (first form))))
                                  (list op-name
                                        (if (member form macros)
                                            :macro
                                            :compiler-macro)
                                        start
                                        (position-line start pretty-expansion)
                                        (length op-name))))))))))

(defun position-line (position string)
  (let ((line 0)
        (last-newline-position 0))
    (loop for i upto position
          for char across string
          when (eql char #\Newline)
            do (incf line)
               (setq last-newline-position i))
    line))

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

;;;; Tracking Pretty Printer

(defun marker-char-p (char)
  (<= #xe000 (char-code char) #xe8ff))

(defun make-marker-char (id)
  ;; using the private-use characters U+E000..U+F8FF as markers, so
  ;; that's our upper limit for how many we can use.
  (assert (<= 0 id #x8ff))
  (code-char (+ #xe000 id)))

(defun marker-char-id (char)
  (assert (marker-char-p char))
  (- (char-code char) #xe000))

(defun make-tracking-pprint-dispatch (forms)
  (let ((original-table *print-pprint-dispatch*)
        (table (copy-pprint-dispatch)))
    (flet ((maybe-write-marker (position stream)
             (when position
               (write-char (make-marker-char position) stream))))
      (set-pprint-dispatch 'cons
                           (lambda (stream cons)
                             (let ((pos (position cons forms)))
                               (maybe-write-marker pos stream)
                               ;; delegate printing to the original table.
                               (funcall (pprint-dispatch cons original-table)
                                        stream
                                        cons)
                               (maybe-write-marker pos stream)))
                           most-positive-fixnum
                           table))
    table))

(defparameter +whitespace+ (mapcar #'code-char '(9 13 10 32)))

(defun whitespacep (char)
  (member char +whitespace+))

(defun collect-marker-positions (string position-count)
  (let ((positions (make-array position-count :initial-element nil)))
    (loop with p = 0
          for char across string
          unless (whitespacep char)
            do (if (marker-char-p char)
                   (push p (aref positions (marker-char-id char)))
                   (incf p)))
    (map 'list #'reverse positions)))

(defun find-non-whitespace-position (string position)
  (loop with non-whitespace-position = -1
        for i from 0 and char across string
        unless (whitespacep char)
          do (incf non-whitespace-position)
        until (eql non-whitespace-position position)
        finally (return i)))

(defun collect-form-positions (expansion printed-expansion forms)
  (loop for (start end)
          in (collect-marker-positions
              (pprint-to-string expansion (make-tracking-pprint-dispatch forms))
              (length forms))
        collect (when (and start end)
                  (list (find-non-whitespace-position printed-expansion start)
                        (find-non-whitespace-position printed-expansion end)))))

(provide :swank-macrostep)
