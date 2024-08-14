(unless rc-posix-p
  (throw 'return t))

(use-package app-launcher
  :straight
  (:host github :repo "NumericalGandalf/app-launcher")
  :commands
  app-launcher)

(defun app-launcher ()
  "Create new minibuffer-only frame and call `app-launcher-run-app'."
  (interactive)
  (let* ((height 25)
         (vertico-count (1- height))
         (ivy-height height))
    (with-selected-frame
        (make-frame `((name . "app-launcher")
                      (minibuffer . only)
		              (height . ,height)
                      (width . ,(* height 4))))
      (unwind-protect
	      (app-launcher-run-app t)
        (delete-frame)))))

(use-package guix)

(use-package vterm
  :custom
  (vterm-timer-delay 0.01)
  (vterm-clear-scrollback-when-clearing t)
  (vterm-always-compile-module t))
