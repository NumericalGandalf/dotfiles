(require 'use-package)
(setq use-package-always-ensure t)

(use-package no-littering
  :init
  (setq no-littering-etc-directory user-emacs-directory))

(use-package diminish)

(defun rc-ensure-theme ()
  (when (display-graphic-p)
    (let ((theme 'nord))
      (unless (member theme custom-enabled-themes)
        (load-theme theme t)))))

(add-hook
  (if (daemonp)
    (progn
      (setq initial-buffer-choice default-directory)
      'server-after-make-frame-hook)
    (progn
      (unless (buffer-file-name) (find-file default-directory))
      'emacs-startup-hook))
  'rc-ensure-theme)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode 0)
(fringe-mode 0)

(setq use-dialog-box nil
  overflow-newline-into-fringe nil
  use-short-answers t
  inhibit-startup-message t
  echo-keystrokes 0
  mode-line-percent-position '(6 "%q"))

(setq-default resize-mini-windows t
  cursor-in-non-selected-windows nil)

(let ((rc-font "Hack Nerd Font Mono-11"))
  (set-face-attribute 'default nil :font rc-font)
  (add-to-list 'default-frame-alist `(font . ,rc-font)))

(global-display-line-numbers-mode 1)
(column-number-mode 1)
(setq display-line-numbers-type 'relative
  display-line-numbers-width-start t)

(global-set-key (kbd "C-c M-m") 'man)
(global-set-key (kbd "C-c M-t") 'ansi-term)

(savehist-mode 1)
(recentf-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)

(setq enable-recursive-minibuffers t
  isearch-repeat-on-direction-change t
  global-auto-revert-non-file-buffers t)

(setq make-backup-files nil
  create-lockfiles nil
  custom-file (locate-user-emacs-file "var/void.el"))

(setq completion-ignore-case t
  read-file-name-completion-ignore-case t
  read-buffer-completion-ignore-case t)

(use-package orderless
  :config
  (add-to-list 'completion-styles 'orderless))

(use-package marginalia
  :diminish
  :config
  (marginalia-mode 1))

(use-package vertico
  :config
  (vertico-mode 1))

(use-package consult
  :config
  (setq completion-in-region-function 'consult-completion-in-region)
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap project-switch-to-buffer] 'consult-project-buffer)
  (global-set-key [remap imenu] 'consult-imenu)
  (global-set-key [remap bookmark-jump] 'consult-bookmark))

(with-eval-after-load 'dired
  (setq dired-listing-switches "-lah"
    dired-free-space 'separate
    dired-recursive-deletes 'always
    dired-kill-when-opening-new-dired-buffer t
    dired-auto-revert-buffer t
    auth-source-save-behavior nil))

(setq compile-command ""
  compilation-ask-about-save nil
  compilation-scroll-output 'first-error
  find-ls-option '("-exec ls -ldh {} +" . "-ldh"))

(global-set-key (kbd "C-c b c") 'compile)
(global-set-key (kbd "C-c k c") 'kill-compilation)

(defun rc--read-command (prompt history &optional directory)
  (let ((default-directory (if directory directory default-directory)))
    (save-some-buffers t)
    (read-shell-command prompt (car (symbol-value history)) `(,history . 1))))

(defun rc--project-root ()
  (require 'project)
  (project-root (project-current t)))

(defun rc-find-buffer-close ()
  (interactive)
  (if (one-window-p) (quit-window) (delete-window)))

(defun rc--find (directory find-expr)
  (split-window-right)
  (other-window 1)
  (find-dired-with-command directory (concat find-expr " " (car find-ls-option)))
  (with-current-buffer "*Find*"
    (use-local-map (copy-keymap dired-mode-map))
    (local-set-key [remap quit-window] 'rc-find-buffer-close)
    (local-set-key [remap dired-find-file] 'dired-find-file-other-window)))

(defun rc-find (find-expr)
  (interactive
    (list (rc--read-command "Find command: " 'find-command-history)))
  (rc--find default-directory find-expr))

(global-set-key (kbd "C-x p C-c") 'rc-project-find)
(global-set-key (kbd "C-c b f") 'rc-find)
(global-set-key (kbd "C-c k f") 'kill-find)

(defun rc-project-find ()
  (interactive)
  (let* ((directory (rc--project-root))
          (find-expr (rc--read-command "Find command: " 'find-command-history directory)))
    (rc--find directory find-expr)))

(defun rc-grep (grep-expr)
  (interactive
    (list (rc--read-command "Grep command: " 'grep-history)))
  (grep grep-expr))

(defun rc-project-grep ()
  (interactive)
  (let* ((directory (rc--project-root))
         (grep-expr (rc--read-command "Grep command: " 'grep-history directory)))
    (let ((default-directory directory))
      (grep grep-expr))))

(global-set-key (kbd "C-c b g") 'rc-grep)
(global-set-key (kbd "C-x p C") 'rc-project-grep)
(global-set-key (kbd "C-c k g") 'kill-grep)

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))
    
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
  (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
  (when (package-installed-p 'consult)
    (define-key eglot-mode-map (kbd "C-c l e") 'consult-flymake)))

(use-package magit)

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))
