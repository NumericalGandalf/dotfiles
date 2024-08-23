(use-package app-launcher
  :ensure
  (:host github :repo "NumericalGandalf/app-launcher"))

(use-package vterm
  :custom
  (vterm-clear-scrollback-when-clearing t)
  (vterm-always-compile-module t)
  :config
  (setq vterm-timer-delay nil))

(use-package guix)

(defun run-browser ()
  "Run env browser."
  (interactive)
  (when-let ((browser (getenv "BROWSER")))
    (call-process-shell-command browser nil 0 nil)))

(defun run-terminal ()
  "Run env terminal or `vterm'."
  (interactive)
  (if-let ((terminal (getenv "TERMINAL")))
      (call-process-shell-command terminal nil 0 nil)
    (with-selected-frame (make-frame)
      (vterm))))

(provide 'applications)
