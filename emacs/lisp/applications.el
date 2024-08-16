(use-package app-launcher
  :straight
  (:host github :repo "NumericalGandalf/app-launcher"))

(use-package guix)

(use-package vterm
  :custom
  (vterm-timer-delay 0.01)
  (vterm-clear-scrollback-when-clearing t)
  (vterm-always-compile-module t))

(defun browser-run ()
  "Run XDG default web browser."
  (interactive)
  (let ((browser
         "gtk-launch $(xdg-settings get default-web-browser)"))
    (call-process-shell-command browser nil 0 nil)))

(use-package chess)

(provide 'applications)
