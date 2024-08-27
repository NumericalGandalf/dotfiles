(defgroup posix nil
  "Posix OS Management."
  :prefix "sys-"
  :group 'emacs)

(use-package app-launcher
  :ensure
  (:host github :repo "NumericalGandalf/app-launcher"))

(use-package vterm
  :general
  (vterm-mode-map
   "C-j" (lambda ()
           (interactive)
           (vterm-send "C-c")))
  :config
  (setq vterm-timer-delay nil)
  :custom
  (vterm-clear-scrollback-when-clearing t)
  (vterm-always-compile-module t))

(use-package guix
  :general
  ("C-c I" 'guix))

(use-package restart-emacs
  :general
  (:prefix "C-c r"
           "r" 'restart-emacs
           "R" 'restart-emacs-start-new-emacs))

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

(provide 'posix)
