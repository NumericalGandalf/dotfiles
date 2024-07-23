(add-to-list 'load-path (locate-user-emacs-file "modules/"))

(require 'rc-base)

(setq custom-file (rc-var-file "custom.el"))
(load custom-file t t)

(require 'rc-packages)
(require 'rc-theming)
(require 'rc-dotfiles)
(require 'rc-minibuffer)
(require 'rc-applications)
(require 'rc-editing)
(require 'rc-programming)
(require 'rc-keybindings)
