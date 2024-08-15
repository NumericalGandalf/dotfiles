(use-package app-launcher
  :straight
  (:host github :repo "NumericalGandalf/app-launcher")
  :config
  (define-advice app-launcher-frame
      (:around (fun &rest args) completion-heights)
    (let ((vertico-count (1- app-launcher-frame-height)))
      (apply fun args))))

(use-package guix)

(use-package vterm
  :custom
  (vterm-timer-delay 0.01)
  (vterm-clear-scrollback-when-clearing t)
  (vterm-always-compile-module t))

(provide 'applications)
