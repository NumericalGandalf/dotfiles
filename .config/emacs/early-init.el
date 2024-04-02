(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
    (expand-file-name "/var/eln-cache/" user-emacs-directory)))

(with-eval-after-load 'use-package
  (require 'package)
  (setq package-user-dir (locate-user-emacs-file "var/elpa/")
    package-gnupghome-dir (locate-user-emacs-file "var/elpa/gnupg/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  (unless package-archive-contents (package-refresh-contents t)))
