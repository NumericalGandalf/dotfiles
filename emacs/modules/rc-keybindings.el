(use-package general
  :demand
  :config
  (general-define-key
   "M-y" 'consult-yank-pop
   "C-s" 'consult-line
   
   "C-r" 'rc-sudo-buffer
   "C-q" 'rc-duplicate-line
   
   "M-P" 'move-text-up
   "M-N" 'move-text-down

   "C->" 'mc/mark-next-like-this
   "C-<" 'mc/unmark-next-like-this)

  (general-define-key
   :prefix "C-x"
   "C-b" 'ibuffer-other-window
   
   "M-:" 'consult-complex-command
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

   "o ." 'rc-open-init-file
   "o ," 'rc-open-cache-dir
   "o /" 'dots-open-files
   "o ?" 'dots-open-assets

   "o p" 'list-packages
   "o P" 'use-package-report

   "r m" 'consult-man
   "r f" 'consult-find
   "r g" 'consult-grep
   "r y" 'consult-git-grep
   
   "r !" 'shell-command
   "r &" 'async-shell-command
   "r %" 'query-replace-regexp
   "r c" 'compile
   
   "r a" 'guix
   "r t" 'vterm

   "f r" 'recentf
   "f l" 'find-library)

  (general-def minibuffer-local-map
    "M-r" 'consult-history
    
    "C-." 'embark-act
    "C-;" 'embark-dwin
    "C-h B" 'embark-bindings)

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

(provide 'rc-keybindings)
