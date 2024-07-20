(unless (package-installed-p 'app-launcher)
  (package-vc-install
   '(app-launcher :url "https://github.com/SebastienWae/app-launcher.git")))

(defcustom app-launcher-frame-height 20
  "Height of `app-launcher' frame."
  :type 'natnum)

(defun app-launcher ()
  "Creates new minibuffer-only frame and calls `app-launcher-run-app'."
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
  :config
  (setq vterm-timer-delay 0.01
	vterm-max-scrollback 10000
	vterm-clear-scrollback-when-clearing t)
  (define-key vterm-mode-map (kbd "C-j") 'vterm-send-C-c))

(provide 'apps)
