(with-eval-after-load 'package
  (setq
    package-user-dir "~/.cache/emacs/elpa/"
    package-gnupghome-dir "~/.cache/emacs/gnupg/")
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
(global-visual-line-mode 1)

(let ((font "DejaVu Sans Mono"))
  (set-face-attribute 'default t :font font :height 150)
  (add-to-list 'default-frame-alist `(font . ,font))
  (set-face-attribute 'fixed-pitch nil :family font)
  (set-face-attribute 'fixed-pitch-serif nil :family font)
  (set-face-attribute 'variable-pitch nil :family font))

(load-theme 'zenburn t)
