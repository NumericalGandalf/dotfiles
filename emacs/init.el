(add-to-list 'load-path (locate-user-emacs-file "modules/"))

(require 'rc-base)
(require 'rc-dotfiles)
(require 'rc-packages)
(require 'rc-theming)
(require 'rc-minibuffer)
(require 'rc-editing)
(require 'rc-programming)
(require 'rc-applications)
(require 'rc-keybindings)

(load custom-file t t)
