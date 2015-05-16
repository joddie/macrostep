
(defpackage swank-macrostep
  (:use cl swank)
  (:import-from swank
                #:with-buffer-syntax
                #:to-string
                #:macroexpand-all)
  (:export #:macrostep-expand-1
           #:macro-form-p))

(in-package #:swank-macrostep)

(defun macrostep-expand-1 (string binding-strings &optional compiler-macros?)
  (with-buffer-syntax ()
    (let* ((form (read-from-string string))
           (bindings (mapcar #'read-from-string binding-strings))
           (env (compute-environment bindings)))
      (case (macro-form-type form env compiler-macros?)
        (macro
         (multiple-value-bind (expansion substitutions)
             (substitute-macros (macroexpand-1 form env))
           (list
            (to-string expansion)
            (loop for (replacement . original) in substitutions
                 collect (cons (to-string replacement)
                               (to-string original))))))
        (compiler-macro
         (error "Not implemented"))
        (t
         (error "Not a macro form"))))))

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
     'macro)
    ((and compiler-macros?
          (compiler-macro-function (car sexp) environment))
     'compiler-macro)
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

(defun substitute-macros (form)
  (let ((forms (collect-macro-forms form)))
    (substitute-macro-forms form forms)))

(defun collect-macro-forms (form)
  (let* ((real-macroexpand-hook *macroexpand-hook*)
         (macro-forms '())
         (*macroexpand-hook*
          (lambda (macro-function form environment)
            (setq macro-forms
                  (cons form macro-forms))
            (funcall real-macroexpand-hook macro-function form environment))))
    (macroexpand-all form)
    macro-forms))

(defun substitute-macro-forms (form forms)
  (cond ((not (consp form))
         (values form nil))
        ((member form forms :test 'eq)
         (multiple-value-bind (rest replacements)
             (substitute-macro-forms (cdr form) forms)
           (let ((replacement (gensym)))
             (values
              (cons replacement rest)
              (cons (cons replacement (car form))
                    replacements)))))
        (t
         (multiple-value-bind (car replacements-1)
             (substitute-macro-forms (car form) forms)
           (multiple-value-bind (cdr replacements-2)
               (substitute-macro-forms (cdr form) forms)
             (values
              (cons car cdr)
              (append replacements-1 replacements-2)))))))
