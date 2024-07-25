(add-to-list 'load-path (locate-user-emacs-file "modules/"))

(require 'rc-base)
(require 'rc-dotfiles)

(setq custom-file (rc-cache-file "custom.el"))

(require 'rc-packages)
(require 'rc-theming)
(require 'rc-minibuffer)
(require 'rc-applications)
(require 'rc-editing)
(require 'rc-programming)
(require 'rc-keybindings)

(load custom-file t t)
