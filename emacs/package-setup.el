(require 'package)
(require 'use-package)

(defcustom package-auto-upgrade-interval 7
  "Interval in days for package auto upgrading."
  :type 'natnum)

(defcustom package-auto-upgrade-hook nil
  "Hook to run after `package-auto-upgrade'."
  :type 'hook)

(setq package-user-dir (rc/cache "packages/")
      package-gnupghome-dir (rc/expand "gnupg/" package-user-dir))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defun package-auto-upgrade (&optional prefix)
  "Upgrade packages if `package-auto-upgrade-interval' has passed.
If optional PREFIX is non-nil, force the upgrade.

Also run `package-auto-upgrade-hook' after the upgrade."
  (interactive "P")
  (let ((file (rc/expand "next-upgrade" package-user-dir))
        (day (time-to-days (current-time)))
        (vc--inhibit-async-window t))
    (when (or prefix
              (not (file-exists-p file))
              (>= day (with-temp-buffer
                        (insert-file-contents file)
                        (string-to-number (buffer-string)))))
      (package-upgrade-all)
      (with-temp-file file
        (erase-buffer)
        (insert (int-to-string (+ day package-auto-upgrade-interval))))
      (run-hooks 'package-auto-upgrade-hook))))

(add-hook 'after-init-hook #'package-auto-upgrade)

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-hook-name-suffix nil)

(when init-file-debug
  (setq use-package-verbose t
        use-package-compute-statistics t))

(use-package diminish)

(use-package no-littering
  :demand
  :init
  (setq no-littering-etc-directory (rc/expand)
        no-littering-var-directory (rc/cache))
  :custom
  (server-auth-dir (rc/cache "server/")))
