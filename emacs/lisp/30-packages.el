(defcustom package-auto-upgrade-interval 7
  "Interval in days for package auto upgrading."
  :type 'natnum)

(require 'package)
(require 'use-package)

(setq package-user-dir (rc-cache "elpa/")
      package-gnupghome-dir (rc-expand "gnupg/" package-user-dir))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless package-enable-at-startup
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-hook-name-suffix nil
      use-package-compute-statistics t)

(setq straight-base-dir (rc-cache)
      straight-enable-package-integration nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package no-littering
  :demand
  :init
  (setq no-littering-etc-directory (rc-expand)
        no-littering-var-directory (rc-cache)))

(defun package-auto-upgrade (&optional prefix)
  "Upgrade packages if `package-auto-upgrade-interval' has passed.
If PREFIX is non-nil, force upgrade."
  (interactive "P")
  (let ((file (rc-cache "last-upgrade"))
        (day (time-to-days (current-time))))
    (when (or prefix
              (not (file-exists-p file))
              (<= package-auto-upgrade-interval
                  (- day (string-to-number
                          (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string))))))
      (package-upgrade-all nil)
      (straight-pull-all)
      (rc-file file
        (insert (int-to-string day))))))

(add-hook 'after-init-hook 'package-auto-upgrade)
