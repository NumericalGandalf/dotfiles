(use-package app-launcher
  :straight (:host github :repo "SebastienWae/app-launcher")
  :commands (app-launcher))

(defun app-launcher ()
  "Create new minibuffer-only frame and call `app-launcher-run-app'."
  (interactive)
  (with-selected-frame
      (make-frame `((name . "app-launcher")
                    (minibuffer . only)
		    (height . ,app-launcher-frame-height)))
    (unwind-protect
	(let ((vertico-count app-launcher-frame-height))
	  (app-launcher-run-app))
      (delete-frame))))

(use-package vterm
  :defer
  :custom
  (vterm-timer-delay 0.01)
  (vterm-max-scrollback 10000)
  (vterm-clear-scrollback-when-clearing t)
  (vterm-always-compile-module t))

(provide 'rc-applications)
