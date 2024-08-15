(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :custom
  (evil-toggle-key "C-x C-z")
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump t)
  (evil-want-fine-undo t)
  (evil-undo-system 'undo-redo))

(use-package evil-collection
  :after
  evil
  :demand
  :config
  (evil-collection-init))

(use-package general
  :demand
  :config
  (general-define-key
   "M-y" 'consult-yank-pop
   "C-s" 'consult-line
   "C-z" 'evil-mode

   "C-S-N" 'next-buffer
   "C-S-P" 'previous-buffer
   
   "C-q" 'rc-duplicate-line
   
   "M-P" 'move-text-up
   "M-N" 'move-text-down

   "C->" 'mc/mark-next-like-this
   "C-<" 'mc/unmark-next-like-this)

  (general-define-key
   :prefix "C-x"
   "C-b" 'ibuffer-other-window
   "b" 'consult-buffer   
   "4 b" 'consult-buffer-other-window
   "5 b" 'consult-buffer-other-frame
   "r b" 'consult-bookmark)

  (general-define-key
   :prefix "C-h"
   "f" 'helpful-callable
   "v" 'helpful-variable
   "k" 'helpful-key
   "C-." 'helpful-at-point)

  (general-define-key
   :prefix "M-g"
   "e" 'consult-compile-error
   "o" 'consult-outline
   "i" 'consult-imenu)

  (general-define-key
   :prefix "C-c"
   "C->" 'mc/mark-all-like-this
   "C-<" 'mc/edit-lines

   "o ." (lambda ()
           (interactive)
           (find-file (file-truename user-init-file)))
   "o ," (lambda ()
           (interactive)
           (find-file (rc-cache)))

   "o p" 'list-packages
   "o P" 'use-package-report
   
   "r c" 'compile
   "r e" 'eshell
   "r !" 'shell-command+
   "r &" 'async-shell-command
   
   "r %" 'query-replace-w-options

   "f r" 'recentf
   "f l" 'find-library
   "f z" 'load-file

   "b r" (lambda ()
           (interactive)
           (revert-buffer t t))

   "w h" 'buf-move-left
   "w j" 'buf-move-down
   "w k" 'buf-move-up
   "w l" 'buf-move-right)

  (when rc-posix-p
    (general-define-key
     "C-r" 'sudo-edit
     "C-S-r" 'sudo-edit-find-file)
    
    (general-define-key
     :prefix "C-c"
     "r m" 'consult-man
     "r f" 'consult-find
     "r g" 'consult-grep
     "r y" 'consult-git-grep

     "o /" (lambda ()
             (interactive)
             (find-file (dots-expand)))
     
     "r a" 'guix
     "r t" 'vterm))

  (general-def minibuffer-local-map
    "M-r" 'consult-history
    
    "C-." 'embark-act
    "C-;" 'embark-dwin
    "C-h B" 'embark-bindings)

  (general-def use-package-statistics-mode-map
    "g" 'use-package-report)

  (general-def grep-mode-map
    "C-c C-p" 'wgrep-change-to-wgrep-mode)

  (general-def corfu-map
    "RET" nil)

  (general-def lsp-mode-map
    "M-?" 'consult-lsp-symbols
    "C-h ." 'lsp-ui-doc-toggle
    "M-g i" 'lsp-ui-imenu
    "M-g f" 'lsp-ui-flycheck-list)

  (general-def vterm-mode-map
    "C-j" 'vterm-send-C-c))

(provide 'keybindings)
