(use-package diminish)

(use-package
  no-littering
  :config (setq no-littering-etc-directory user-emacs-directory))

(with-eval-after-load 'emacs
  (setq
    use-dialog-box nil
    use-short-answers t
    inhibit-startup-message t
    echo-keystrokes 0)
  (setq-default
    resize-mini-windows t
    cursor-in-non-selected-windows nil)
  (setq
    display-line-numbers-type 'relative
    display-line-numbers-width-start t)
  (global-display-line-numbers-mode 1)
  (column-number-mode 1)
  (global-visual-line-mode 1)
  (savehist-mode 1)
  (recentf-mode 1)
  (save-place-mode 1))

(with-eval-after-load 'autorevert
  (setq global-auto-revert-non-file-buffers t
    auto-revert-remote-files t)
  (global-auto-revert-mode 1))

(with-eval-after-load 'isearch
  (setq isearch-repeat-on-direction-change t))

(with-eval-after-load 'minibuffer
  (setq
    enable-recursive-minibuffers t
    completion-ignore-case t
    read-file-name-completion-ignore-case t
    read-buffer-completion-ignore-case t))

(use-package
  orderless
  :config
  (unless (member 'orderless completion-styles)
    (add-to-list 'completion-styles 'orderless)))

(use-package marginalia :diminish :config (marginalia-mode 1))

(use-package vertico :config (vertico-mode 1))

(with-eval-after-load 'dired
  (setq
    dired-listing-switches "-lah"
    find-ls-option '("-exec ls -ldh {} +" . "-ldh")
    dired-free-space 'separate
    dired-recursive-deletes 'always
    dired-kill-when-opening-new-dired-buffer t
    dired-auto-revert-buffer t))

(with-eval-after-load 'tramp
  (setq auth-source-save-behavior nil))

(with-eval-after-load 'compile
  (setq
    compile-command ""
    compilation-ask-about-save nil
    compilation-scroll-output 'first-error))

(with-eval-after-load 'grep
  (grep-apply-setting 'grep-command "")
  (setq grep-save-buffers t
    grep-use-null-device nil))

(use-package wgrep :defer :config (setq wgrep-auto-save-buffer t))

(use-package markdown-mode :defer)

(use-package magit :defer)

(use-package editorconfig :diminish :config (editorconfig-mode 1))

(use-package
  elisp-autofmt
  :defer
  :config (setq elisp-autofmt-style 'fixed))

(use-package
  corfu
  :config
  (setq
    corfu-auto t
    corfu-cycle t)
  (global-corfu-mode 1))

(with-eval-after-load 'xref
  (setq
    xref-auto-jump-to-first-xref t
    xref-auto-jump-to-first-definition t))

(with-eval-after-load 'eglot
  (setq eglot-ignored-server-capabilities
    '
    (:documentHighlightProvider
      :codeLensProvider
      :foldingRangeProvider
      :colorProvider
      :inlayHintProvider))
  (define-key eglot-mode-map (kbd "C-c l d") 'eglot-find-declaration)
  (define-key
    eglot-mode-map
    (kbd "C-c l t")
    'eglot-find-typeDefinition)
  (define-key
    eglot-mode-map
    (kbd "C-c l i")
    'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions))

(defun rc-after-init ()
  (if (daemonp)
    (setq initial-buffer-choice default-directory)
    (unless (buffer-file-name)
      (find-file default-directory))))

(add-hook 'after-init-hook 'rc-after-init)
