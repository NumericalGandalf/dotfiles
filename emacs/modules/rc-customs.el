(defgroup rc nil
  "User Emacs utilities."
  :group 'local
  :prefix "rc-")

(defgroup dots nil
  "Dotfile Management."
  :group 'local
  :prefix "dots-")

(defcustom rc-font "Hack Nerd Font"
  "User nerd font."
  :type 'string)

(defcustom rc-font-asset-name "Hack"
  "Asset name of user font in the nerd-fonts repo."
  :type 'string)

(defcustom rc-font-height 13
  "Default height of the user font."
  :type 'natnum)

(defcustom rc-after-load-font-hook nil
  "Hooks to run after user font gets loaded."
  :type 'hook)

(defcustom dots-deploy-hook nil
  "Hooks to run on dotfiles deploy."
  :type 'hook)

(defcustom dots-sway-font-height-offset -3
  "Offset of sway font height."
  :type 'integer)

(defcustom dots-gtk-font-height-offset -1
  "Offset of gtk font height."
  :type 'integer)

(defcustom dots-waybar-font-height-offset 0
  "Offset of gtk font height."
  :type 'integer)

(defcustom dots-stow-parents '(".config/")
  "List of stow parent directories.

The child directories of these are stowed as they are
and will not be traversed any further.

These directories are relative to the dotfiles dots directory."
  :type '(repeat string))

(defcustom dots-gsettings
  '(("org.gnome.desktop.interface" "font-name" "@FONT")
    ("org.gnome.desktop.interface" "monospace-font-name" "@FONT")
    ("org.gnome.desktop.interface" "gtk-key-theme" "Emacs")
    ("org.gnome.desktop.interface" "color-scheme" "prefer-dark"))
  "List of gsettings in form SCHEME, KEY, VALUE.

Expansions [@]:
    FONT -> concatenated font string."
  :type '(list string string string))

(defcustom app-launcher-frame-height 21
  "Height of `app-launcher' frame."
  :type 'natnum)

(defcustom treesit-ignore-langs '(latex markdown janet)
  "List of languages to ignore in `treesit-ensure-all'."
  :type '(repeat symbol))

(defcustom treesit-user-load-path (rc-cache-file "tree-sitter/")
  "User tree-sitter library load path."
  :type 'string)

(setq custom-file (rc-cache-file "custom.el"))
(load custom-file t t)

(provide 'rc-customs)
