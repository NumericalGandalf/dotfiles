(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
    (locate-user-emacs-file "./var/eln-cache/")))

(with-eval-after-load 'package
  (setq
    package-user-dir (locate-user-emacs-file "./var/elpa/")
    package-gnupghome-dir (locate-user-emacs-file "./var/elpa/gnupg/"))
  (add-to-list
    'package-archives
    '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents t)))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(fringe-mode 0)
(tooltip-mode 0)

(let
  (
    (font "Iosevka")
    (height 115))
  (set-face-attribute 'default t :font font :height height)
  (add-to-list 'default-frame-alist `(font . ,font))
  (set-face-attribute 'fixed-pitch nil :family font)
  (set-face-attribute 'fixed-pitch-serif nil :family font)
  (set-face-attribute 'variable-pitch nil :family font))

(load-theme 'zenburn t)