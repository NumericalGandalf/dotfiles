(require 'package)
(require 'use-package)

(setq package-user-dir (rc-cache-file "elpa/")
      package-gnupghome-dir (rc-expand "gnupg/" package-user-dir))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-compute-statistics t)

(use-package no-littering
  :demand
  :init
  (setq no-littering-etc-directory (rc-expand)
        no-littering-var-directory (rc-cache-file)))

(use-package diminish)
(use-package delight)

(setq straight-base-dir (rc-cache-file))
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

(provide 'rc-packages)
