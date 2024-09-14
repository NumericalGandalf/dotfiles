(require 'package)
(require 'use-package)

(defcustom package-auto-upgrade-interval 7
  "Interval in days for package auto upgrading."
  :type 'natnum)

(setq package-user-dir (rc-cache "packages/")
      package-gnupghome-dir (rc-expand "gnupg/" package-user-dir))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless package-enable-at-startup
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(defun package-auto-upgrade (&optional prefix)
  "Upgrade packages if `package-auto-upgrade-interval' has passed.
If optional PREFIX is non-nil, force the upgrade."
  (interactive "P")
  (let ((file (rc-expand "next-upgrade" package-user-dir))
        (day (time-to-days (current-time))))
    (when (or prefix
              (not (file-exists-p file))
              (>= day (string-to-number
                       (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string)))))
      (package-upgrade-all)
      (package-autoremove)
      (with-temp-file file
        (erase-buffer)
        (insert (int-to-string (+ day package-auto-upgrade-interval)))))))

(add-hook 'after-init-hook 'package-auto-upgrade)

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-hook-name-suffix nil)

(when init-file-debug
  (setq use-package-verbose t
        use-package-compute-statistics t))

(use-package no-littering
  :init
  (setq no-littering-etc-directory (rc-expand)
        no-littering-var-directory (rc-cache)
        server-auth-dir (no-littering-expand-var-file-name "server/")))

(provide 'package-setup)
