;;; macrostep-test.el --- tests for macrostep.el

;; Copyright (C) 2014 Jon Oddie <j.j.oddie@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.


(defmacro macrostep-with-text (object &rest forms)
  (declare (indent 1))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (let ((print-level nil)
           (print-length nil)
           (standard-output (current-buffer)))
       (save-excursion
         (print ,object))
       ,@forms)))

(defmacro macrostep-should-expand (form expansion)
  `(save-excursion
     (goto-char (point-min))
     (let ((print-level nil)
           (print-length nil))
       (search-forward (prin1-to-string ,form))
       (goto-char (match-beginning 0))
       (unwind-protect
            (progn
              (macrostep-expand)
              (should
               (equal (read (current-buffer))
                      ,expansion)))
         (macrostep-collapse-all)))))

(ert-deftest macrostep-expand-defmacro ()
  (defmacro macrostep-dummy-macro (&rest args)
    `(expansion of ,@args))
  (macrostep-with-text
   '(progn
     (first body form)
     (second body form)
     (macrostep-dummy-macro (first (argument)) second (third argument))
     (remaining body forms))
   (macrostep-should-expand
    '(macrostep-dummy-macro (first (argument)) second (third argument))
    '(expansion of (first (argument)) second (third argument)))))

(ert-deftest macrostep-expand-macrolet ()
  (macrostep-with-text
      '(macrolet
        ((test (&rest args) `(expansion of ,@args)))
        (first body form)
        (second body form)
        (test (strawberry pie) and (apple pie))
        (final body form))
    (macrostep-should-expand
     '(test (strawberry pie) and (apple pie))
     '(expansion of (strawberry pie) and (apple pie)))))

(ert-deftest macrostep-expand-macrolet-2 ()
  (macrostep-with-text
      ;; Taken from org-notify.el.
      '(macrolet ((get (k) `(plist-get list ,k))
                  (pr (k v) `(setq result (plist-put result ,k ,v))))
        (let* ((list (nth 1 heading))      (notify (or (get :notify) "default"))
               (deadline (org-notify-convert-deadline (get :deadline)))
               (heading (get :raw-value))
               result)
          (when (and (eq (get :todo-type) 'todo) heading deadline)
            (pr :heading heading)     (pr :notify (intern notify))
            (pr :begin (get :begin))
            (pr :file (nth org-notify-parse-file (org-agenda-files 'unrestricted)))
            (pr :timestamp deadline)  (pr :uid (md5 (concat heading deadline)))
            (pr :deadline (- (org-time-string-to-seconds deadline)
                             (org-float-time))))
          result))
    (macrostep-should-expand
     '(pr :heading heading)
     '(setq result (plist-put result :heading heading)))
    (macrostep-should-expand
     '(pr :notify (intern notify))
     '(setq result (plist-put result :notify (intern notify))))
    (macrostep-should-expand
     '(pr :begin (get :begin))
     '(setq result (plist-put result :begin (get :begin))))
    (macrostep-should-expand
     '(get :begin)
     '(plist-get list :begin))))

(ert-deftest macrostep-expand-cl-macrolet ()
  (macrostep-with-text
      ;; Taken from slime.el.
      '(cl-macrolet ((fontify (face string)
                      `(slime-inspector-fontify ,face ,string)))
        (slime-propertize-region
         (list 'slime-part-number id
          'mouse-face 'highlight
          'face 'slime-inspector-value-face)
         (insert title))
        (while (eq (char-before) ?\n)
          (backward-delete-char 1))
        (insert "\n" (fontify label "--------------------") "\n")
        (save-excursion
          (slime-inspector-insert-content content))
        (when point
          (cl-check-type point cons)
          (ignore-errors
            (goto-char (point-min))
            (forward-line (1- (car point)))
            (move-to-column (cdr point)))))
    (macrostep-should-expand
     '(fontify label "--------------------")
     '(slime-inspector-fontify label "--------------------"))))

(ert-deftest macrostep-expand-shadowed-macrolet ()
  (macrostep-with-text
      '(macrolet
        ((test-macro (&rest forms) (cons 'shadowed forms))
         (test-macro (&rest forms) (cons 'outer-definition forms)))
        (test-macro first (call))
        (cl-macrolet
            ((test-macro (&rest forms) (cons 'inner-definition forms)))
          (test-macro (second (call)))))
    (macrostep-should-expand
     '(test-macro first (call))
     '(outer-definition first (call)))
    (macrostep-should-expand
     '(test-macro (second (call)))
     '(inner-definition (second (call))))))

(ert-deftest macrostep-environnment-at-point ()
  (macrostep-with-text
      ;; Taken from org-notify.el.
      '(macrolet ((get (k) `(plist-get list ,k))
                  (pr (k v) `(setq result (plist-put result ,k ,v))))
        (body forms))
    (search-forward "(body")
    (let ((env (macrostep-environment-at-point)))
      (should (assq 'get env))
      (should (assq 'pr env))
      (should (functionp (cdr (assq 'get env))))
      (should (functionp (cdr (assq 'pr env))))
      (should
       (equal
        (apply (cdr (assq 'pr env)) '(:heading heading))
        '(setq result (plist-put result :heading heading))))
      (should
       (equal
        (apply (cdr (assq 'get env)) '(:begin))
        '(plist-get list :begin))))))

(when noninteractive
  (load-file (expand-file-name "macrostep.el"
                               (file-name-directory load-file-name)))
  (ert-run-tests-batch "^macrostep"))
