(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache "var/eln-cache/"))

(require 'server)
(unless (or (server-running-p) (daemonp))
  (server-start))

(setq custom-file (locate-user-emacs-file "var/custom.el"))
(load custom-file t t)

(require 'package)
(require 'use-package)
(setq package-user-dir (locate-user-emacs-file "var/elpa/")
      package-gnupghome-dir (expand-file-name "gnupg/" package-user-dir)
      use-package-always-ensure t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(use-package no-littering)

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

(require 'candy)
(require 'goodies)
(require 'minibuf)
(require 'prog)
(require 'apps)
(require 'rc)
(require 'dots)
