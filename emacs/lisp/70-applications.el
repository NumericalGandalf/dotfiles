(use-package app-launcher
  :if
  rc-posix-p
  :straight (:host github :repo "NumericalGandalf/app-launcher")
  :commands (app-launcher))

(rc-fun (app-launcher)
    rc-posix-p
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

(use-package guix
  :if
  rc-posix-p)

(use-package vterm
  :if
  rc-posix-p
  :custom
  (vterm-timer-delay 0.01)
  (vterm-clear-scrollback-when-clearing t)
  (vterm-always-compile-module t))
