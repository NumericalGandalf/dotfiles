(defgroup posix nil
  "Posix OS Management."
  :prefix "sys-"
  :group 'emacs)

(use-package app-launcher
  :ensure
  (:fetcher github :repo "NumericalGandalf/app-launcher"))

(use-package vterm
  :custom
  (vterm-clear-scrollback-when-clearing t)
  (vterm-always-compile-module t)
  :config
  (setq vterm-timer-delay nil))

(use-package guix)

(use-package restart-emacs)

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
