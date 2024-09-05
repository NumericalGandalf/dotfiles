(defgroup posix nil
  "Posix OS Management."
  :prefix "posix-"
  :group 'emacs)

(defcustom posix-launcher-height 25
  "Height of `posix-launcher' frame."
  :type 'natnum)

(use-package vterm
  :general
  ("C-c r t" 'vterm)
  (vterm-mode-map
   "C-j" (lambda ()
           (interactive)
           (vterm-send "C-c")))
  :config
  (setq vterm-timer-delay nil)
  :custom
  (vterm-max-scroll-back 5000)
  (vterm-clear-scrollback-when-clearing t)
  (vterm-always-compile-module t))

(use-package guix
  :general
  ("C-c y g" 'guix))

(use-package restart-emacs
  :general
  (:prefix "C-c r"
           "r" 'restart-emacs
           "R" 'restart-emacs-start-new-emacs))

(use-package app-launcher
  :ensure
  (:host github :repo "NumericalGandalf/app-launcher")
  :defer)

(defmacro posix-program (program &optional doc &rest body)
  "Define function for PROGRAM with DOC and BODY.

This will check for an environment variable PROGRAM first
and, if non-nil, run it, or execute BODY otherwise."
  (declare (indent 1) (doc-string 2))
  (let ((name (symbol-name program)))
    `(defun ,(intern (concat "posix-run-" name)) ()
       ,(if (stringp doc)
            doc
          (format "Run posix %s." name))
       (interactive)
       (if-let ((command (getenv (upcase ,name))))
           (call-process-shell-command command nil 0 nil)
         (ignore-errors
           ,@body)))))

(posix-program launcher
  "Run posix launcher or create frame for `app-launcher-run-app'."
  (with-selected-frame
      (make-frame `((name . "posix-launcher")
                    (height . ,posix-launcher-height)
                    (width . ,(* posix-launcher-height 4))
                    (minibuffer . only)))
    (unwind-protect
        (let ((vertico-count (1- posix-launcher-height)))
          (app-launcher-run-app t))
      (delete-frame))))

(posix-program browser)

(posix-program terminal
  "Run posix terminal or `vterm'."
  (with-selected-frame (make-frame)
    (vterm)))

(general-define-key
 :prefix "C-c r"
 "m" 'consult-man
 "f" 'consult-find
 "g" 'consult-grep
 "y" 'consult-git-grep)

(provide 'posix-setup)
