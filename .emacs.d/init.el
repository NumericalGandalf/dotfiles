(use-package diminish)

(use-package no-littering
  :init
  (setq no-littering-etc-directory user-emacs-directory))

(setq use-dialog-box nil
  overflow-newline-into-fringe nil
  use-short-answers t
  inhibit-startup-message t
  initial-scratch-message nil
  echo-keystrokes 0)

(setq-default resize-mini-windows t
  cursor-in-non-selected-windows nil)

(savehist-mode 1)
(recentf-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)

(setq enable-recursive-minibuffers t
  isearch-repeat-on-direction-change t
  global-auto-revert-non-file-buffers t
  auto-revert-remote-files t)

(setq make-backup-files nil
  create-lockfiles nil
  custom-file (locate-user-emacs-file "var/void.el"))

(setq completion-ignore-case t
  read-file-name-completion-ignore-case t
  read-buffer-completion-ignore-case t)

(use-package orderless
  :config
  (unless (member 'orderless completion-styles)
    (add-to-list 'completion-styles 'orderless)))

(use-package marginalia
  :diminish
  :config
  (marginalia-mode 1))

(use-package vertico
  :config
  (vertico-mode 1))

(with-eval-after-load 'dired
  (setq dired-listing-switches "-lah"
    dired-free-space 'separate
    dired-recursive-deletes 'always
    dired-kill-when-opening-new-dired-buffer t
    dired-auto-revert-buffer t))

(with-eval-after-load 'tramp
  (setq auth-source-save-behavior nil))

(setq compile-command ""
  compilation-ask-about-save nil
  compilation-scroll-output 'first-error
  find-ls-option '("-exec ls -ldh {} +" . "-ldh")
  grep-save-buffers t)

(defun rc-find (find-expr)
  (interactive
    (list (progn
            (require 'find-dired)
            (save-some-buffers t)
            (read-shell-command "Find command: " (car find-command-history) '(find-command-history . 1)))))
  (with-selected-window (display-buffer (get-buffer-create "*Find*"))
    (find-dired-with-command default-directory (concat find-expr " " (car find-ls-option)))))

(defun rc-grep (grep-expr)
  (interactive
    (list (read-shell-command "Grep command: " (car grep-history) '(grep-history . 1))))
  (grep grep-expr))

(global-set-key (kbd "C-c b f") 'rc-find)
(global-set-key (kbd "C-c b g") 'rc-grep)

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(use-package magit)

(use-package corfu
  :config
  (setq corfu-cycle t)
  (global-corfu-mode 1))

(setq xref-auto-jump-to-first-xref t
  xref-auto-jump-to-first-definition t)

(global-set-key (kbd "C-c l s") 'eglot)
(with-eval-after-load 'eglot
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider
                                         :inlayHintProvider))
  (define-key eglot-mode-map (kbd "C-c l S") 'eglot-shutdown)
  (define-key eglot-mode-map (kbd "C-c l d") 'eglot-find-declaration)
  (define-key eglot-mode-map (kbd "C-c l t") 'eglot-find-typeDefinition)
  (define-key eglot-mode-map (kbd "C-c l i") 'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions))

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

(defun rc-after-init ()
  (if (daemonp)
    (setq initial-buffer-choice default-directory)
    (unless (buffer-file-name)
      (find-file default-directory))))

(add-hook 'after-init-hook 'rc-after-init)
