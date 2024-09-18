(use-package general
  :defer 0
  :config
  (general-define-key
   "C-S-N" 'next-buffer
   "C-S-P" 'previous-buffer

   "M-y" 'consult-yank-pop
   "C-s" 'consult-line

   "C-r" 'sudo-edit
   "C-S-r" 'sudo-edit-find-file

   "M-P" 'move-text-up
   "M-N" 'move-text-down

   "C->" 'mc/mark-next-like-this
   "C-<" 'mc/unmark-next-like-this)

  (general-define-key
   :prefix "C-x"
   "b" 'consult-buffer
   "4 b" 'consult-buffer-other-window
   "5 b" 'consult-buffer-other-frame
   "r b" 'consult-bookmark
   
   "C-c" 'save-buffers-kill-emacs
   "C-b" 'ibuffer)

  (general-define-key
   :prefix "M-g"
   "e" 'consult-compile-error
   "o" 'consult-outline
   "i" 'consult-imenu)

  (general-define-key
   :prefix "C-c"
   "b r" 'revert-buffer-quick

   "f r" 'recentf
   "f l" 'find-library

   "z l" 'load-file
   "z f" 'font-load
   "z t" 'consult-theme

   "p l" 'package-list-packages
   "p i" 'package-install
   "p d" 'package-delete
   "p r" 'package-reinstall
   "p y" 'use-package-report

   "h f" 'describe-face
   "h g" 'customize-group
   "h t" 'describe-theme

   "r e" 'eshell
   "r f" 'consult-fd
   "r g" 'consult-ripgrep
   "r y" 'consult-git-grep
   "r s" 'async-shell-command
   "r q" 'query-replace-regexp

   "o ." (lambda ()
           (interactive)
           (find-file user-init-file))
   "o ," (lambda ()
           (interactive)
           (find-file (rc/cache)))   
   "o /" (lambda ()
           (interactive)
           (find-file (rc/expand "../")))

   "w h" 'buf-move-left
   "w j" 'buf-move-down
   "w k" 'buf-move-up
   "w l" 'buf-move-right

   "C->" 'mc/mark-all-like-this
   "C-<" 'mc/edit-lines)

  (general-def minibuffer-local-map
    "M-r" 'consult-history
    "C-." 'embark-act
    "C-;" 'embark-dwin
    "C-h B" 'embark-bindings)

  (general-with-eval-after-load 'corfu
	(general-def corfu-map
      "RET" nil
      "SPC" 'corfu-insert-separator))

  (general-with-eval-after-load 'lsp-mode
    (setq lsp-keymap-prefix "C-c l")

	(general-def lsp-mode-map
      "M-?" 'consult-lsp-symbols
      "C-h ." 'lsp-ui-doc-toggle
      "M-g i" 'lsp-ui-imenu
      "M-g f" 'lsp-ui-flycheck-list))

  (when init-file-debug
	(general-define-key
	 :prefix "C-h 4"
	 "u" 'use-package-report
	 "p" 'profiler-report))

  (when rc/posix-p
    (general-define-key
     :prefix "C-c"
     "r m" 'man
     "r t" 'vterm

     "p g" 'guix)

	(general-with-eval-after-load 'vterm
      (general-def vterm-mode-map
		"C-j" (lambda ()
				(interactive)
				(vterm-send "C-c"))))))

(provide 'keybindings)
