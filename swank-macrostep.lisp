
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
                      (error "Not a macro or compiler-macro form."))))))))
      (multiple-value-bind (result substitutions)
          (substitute-macros expansion)
        (list
         (to-string result)
         (loop for (gensym original type) in substitutions
            collect (list (to-string gensym)
                          (to-string original)
                          type)))))))

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

(defun substitute-macros (form)
  (multiple-value-bind (macro-forms compiler-macro-forms)
      (collect-macro-forms form)
    (substitute-macro-forms form macro-forms compiler-macro-forms)))

(defun collect-macro-forms (form)
  (let* ((real-macroexpand-hook *macroexpand-hook*)
         (macro-forms '())
         (*macroexpand-hook*
          (lambda (macro-function form environment)
            (setq macro-forms
                  (cons form macro-forms))
            (funcall real-macroexpand-hook macro-function form environment)))
         (expansion (ignore-errors (macroexpand-all form)))
         (compiler-macro-forms '())
         (*macroexpand-hook*
          (lambda (macro-function form environment)
            (setq compiler-macro-forms
                  (cons form compiler-macro-forms))
            (funcall real-macroexpand-hook macro-function form environment))))
    (ignore-errors
      (compile nil `(lambda () ,expansion)))
    (values macro-forms compiler-macro-forms)))

(defun substitute-macro-forms (form macro-forms compiler-macro-forms)
  (labels
      ((macro-form? (form)
         (member form macro-forms :test 'eq))
       (compiler-macro-form? (form)
         (member form compiler-macro-forms :test 'eq))
       (recur (form)
         (let (macro? compiler-macro?)
           (cond ((not (consp form))
                  (values form nil))
                 ((or
                   (setf macro? (macro-form? form))
                   (setf compiler-macro? (compiler-macro-form? form)))
                  (multiple-value-bind (rest replacements)
                      (recur (cdr form))
                    (let ((replacement (gensym)))
                      (values
                       (cons replacement rest)
                       (cons (list replacement (car form)
                                   (if macro? :macro :compiler-macro))
                             replacements)))))
                 (t
                  (multiple-value-bind (car replacements-1)
                      (recur (car form))
                    (multiple-value-bind (cdr replacements-2)
                        (recur (cdr form))
                      (values
                       (cons car cdr)
                       (append replacements-1 replacements-2)))))))))
    (recur form)))
