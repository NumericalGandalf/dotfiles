(use-package app-launcher
  :ensure
  (:host github :repo "NumericalGandalf/app-launcher")
  :after
  vertico
  :init
  (defun app-launcher (&optional prefix)
    "Create minibuffer-only frame for `app-launcher-run-app'."
    (interactive "P")
    (let* ((height 25)
           (vertico-count (1- height)))
      (with-selected-frame
          (make-frame `((name . "app-launcher")
                        (minibuffer . only)
		                (height . ,height)
                        (width . ,(* height 4))))
        (unwind-protect
	        (app-launcher-run-app prefix)
          (delete-frame))))))

(use-package vterm
  :custom
  (vterm-timer-delay 0.01)
  (vterm-clear-scrollback-when-clearing t)
  (vterm-always-compile-module t))

(use-package guix)

(defun browser-run ()
  "Run default XDG web browser."
  (interactive)
  (let ((browser
         "gtk-launch $(xdg-settings get default-web-browser)"))
    (call-process-shell-command browser nil 0 nil)))

(provide 'applications)
