;;; macrostep-julia --- macrostep interface for julia
;;; Commentary:

;; This relies on an inferior julia process to return macro expansions
;; from julia's `macroexpand' function.  If macros in the current buffer
;; haven't been evaluated in the inferior process they won't be expanded,
;; the same as with macrostepping emacs lisp.

;; The `process-list' is searched for the first available julia process.

;;; Code:
(require 'macrostep)
(declare-function julia-mode "julia-mode")

(eval-when-compile
  (require 'subr-x)

  (unless (fboundp 'ignore-errors)
    (defmacro ignore-errors (&rest body)
      `(condition-case nil (progn ,@body) (error nil)))))

(defcustom macrostep-julia-prompt nil
  "Inferior julia prompt. This is used when parsing output from 
`macroexpand'. If nil, the first bound variable from 
`inferior-ess-primary-prompt', `julia-prompt-regexp', 
`comint-prompt-regexp' will be used, of, if none of those are found, 
\"^\\w+> \"."
  :group 'macrostep
  :type '(choice (const nil) regexp))

(put 'macrostep-julia-non-macro 'error-message
     "Text around point is not a macro call.")
(put 'macrostep-julia-no-process 'error-message
     "No inferior julia process found.")
(put 'macrostep-julia-expansion-failure 'error-message
     "Macro expansion failed.")

;;;###autoload
(defun macrostep-julia-mode-hook ()
  (setq macrostep-sexp-bounds-function          #'macrostep-julia-sexp-bounds)
  (setq macrostep-sexp-at-point-function        #'macrostep-julia-sexp-at-point)
  (setq macrostep-environment-at-point-function #'ignore)
  (setq macrostep-expand-1-function             #'macrostep-julia-expand-1)
  (setq macrostep-print-function                #'macrostep-julia-print-function)
  (add-hook 'macrostep-mode-off-hook #'macrostep-julia-mode-off nil t))

;; buffer to which inferior output is redirected for expansion calls
;; kill it when done with mode
(defvar macrostep-julia-output-buffer "*macrostep julia output*")

(defun macrostep-julia-mode-off (&rest _ignore)
  (when (derived-mode-p 'julia-mode)
    (let ((warning-window
           (get-buffer-window macrostep-julia-output-buffer)))
      (when warning-window
        (quit-window nil warning-window)))))

;;;###autoload
(add-hook 'julia-mode-hook #'macrostep-julia-mode-hook)

;; ------------------------------------------------------------

;; Search back from symbol at point until beginning of line looking for
;; first macro
(defun macrostep-julia-sexp-bounds ()
  (save-excursion
    (let ((bnds (bounds-of-thing-at-point 'symbol))
          (bol (point-at-bol)))
      (while (not (or (< (point) bol)
                      (eq ?@ (char-after (car bnds)))))
        (backward-sexp)
        (setq bnds (bounds-of-thing-at-point 'symbol)))
      (unless (eq ?@ (char-after (car bnds)))
        (signal 'macrostep-julia-non-macro nil))
      (goto-char (car bnds))
      (cond
       ;; (| @macro ... |)
       ((looking-back "(\\s-*" bol)
        (goto-char (match-beginning 0))
        (forward-sexp)
        (cons (car bnds) (1- (point))))
       ;; Push end position forward lines while looking at
       ;; `julia-hanging-operator-regexp' it seems to only be true
       ;; with point is at beginning of line, havent looked closely yet
       (t
        (beginning-of-line)
        (while (looking-at-p julia-hanging-operator-regexp)
          (forward-comment (point-max))
          (forward-line 1)
          (beginning-of-line))
        (cons (car bnds) (point-at-eol)))))))

(defun macrostep-julia-sexp-at-point (start end)
  (cons start end))

;; FIXME: do multiple expansions by quoting inner macros?
(defun macrostep-julia-expand-1 (region _ignore)
  (macrostep-julia-get-expansion
   (buffer-substring-no-properties (car region) (cdr region))))

(defun macrostep-julia-print-function (expansion &rest _ignore)
  (insert expansion))

;; ------------------------------------------------------------
;; Interact with inferior julia process

;; search through process-list for a julia process, ie
;; process command matchs "julia" sans directory / extension
(defvar macrostep-julia-inf-process nil)
(defun macrostep-julia-inf-process ()
  (or (and (process-live-p macrostep-julia-inf-process)
           macrostep-julia-inf-process)
      (setq macrostep-julia-inf-process
            (let ((procs (process-list))
                  found proc)
              (while (and (not found) procs
                          (process-live-p (setq proc (pop procs)))
                          (process-command proc))
                (when (string= "julia"
                               (file-name-sans-extension
                                (file-name-nondirectory
                                 (car (process-command proc)))))
                  (setq found proc)))
              (or found
                  (signal 'macrostep-julia-no-process nil))))))

;; send string to inferior process, redirecting output to
;; `macrostep-julia-output-buffer' temporarily to process string
(defun macrostep-julia-get-expansion (string)
  (when-let ((proc (macrostep-julia-inf-process)))
    (let* ((out-buffer (get-buffer-create macrostep-julia-output-buffer))
           (og-buff (process-buffer proc))
           (og-filt (process-filter proc))
           (og-mark (marker-position (process-mark proc)))
           ;; if `macrostep-julia-prompt' is nil, try ess prompt,
           ;; julia-mode prompt, comint prompt, generic julia prompt
           (prompt (or macrostep-julia-prompt
                       (bound-and-true-p inferior-ess-primary-prompt)
                       (bound-and-true-p julia-prompt-regexp)
                       (bound-and-true-p comint-prompt-regexp)
                       "^\\w*> ")))
      (unwind-protect
          (progn
            (set-process-buffer proc out-buffer)
            (set-process-filter proc 'nil)
            (with-current-buffer out-buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (set-marker (process-mark proc) (point-min))
                (process-send-string
                 proc (format "macroexpand(:(%s))\n" string))
                (sleep-for 0.02)
                ;; FIXME: first time calling doesn't accept all the
                ;; ouput from process
                (accept-process-output proc 1))))
        ;; restore process things
        (set-process-buffer proc og-buff)
        (set-process-filter proc og-filt)
        (set-marker (process-mark proc) og-mark og-buff))
      ;; pull result string from output buffer
      (with-current-buffer out-buffer
        (when (zerop (buffer-size))
          (error "Macro expansion failed: possibly timed out."))
        (goto-char (point-min))
        (when (looking-at-p "ERROR")
          (error "Macro expansion failed: %s"
                 (buffer-substring-no-properties (point-at-bol)
                                                 (point-at-eol))))
        (condition-case nil
            (re-search-forward prompt)
          (error (signal 'macrostep-julia-expansion-failure nil)))
        (forward-line 0)
        (skip-chars-backward " \n\t\r")
        (julia-mode)
        (indent-region (point-min) (point))
        (buffer-substring (point-min) (point))))))

;; find macro definitions in buffer
(defun macrostep-julia-local-macros ()
  (save-excursion
    (goto-char (point-min))
    (let (macros)
      (while (re-search-forward "^[ \t]*macro[ \t]+\\([a-zA-Z0-9]+\\)")
        (push (match-string-no-properties 1) macros))
      macros)))

(provide 'macrostep-julia)
;;; macrostep-julia.el ends here
