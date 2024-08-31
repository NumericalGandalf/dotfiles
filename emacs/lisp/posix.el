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
  (:host github :repo "NumericalGandalf/app-launcher"))

(defun posix-launcher ()
  "Create frame for `app-launcher-run-app'."
  (interactive)
  (with-selected-frame
      (make-frame `((name . "posix-launcher")
                    (height . ,posix-launcher-height)
                    (width . ,(* posix-launcher-height 4))
                    (minibuffer . only)))
    (unwind-protect
        (let ((vertico-count (1- posix-launcher-height)))
          (app-launcher-run-app t))
      (delete-frame))))

(defun posix-browser ()
  "Run system browser."
  (interactive)
  (when-let ((browser (getenv "BROWSER")))
    (call-process-shell-command browser nil 0 nil)))

(defun posix-terminal ()
  "Run system terminal or `vterm'."
  (interactive)
  (if-let ((terminal (getenv "TERMINAL")))
      (call-process-shell-command terminal nil 0 nil)
    (with-selected-frame (make-frame)
      (vterm))))

(general-define-key
 :prefix "C-c r"
 "m" 'consult-man
 "f" 'consult-find
 "g" 'consult-grep
 "y" 'consult-git-grep)

(provide 'posix)
