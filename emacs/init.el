(defun font-apply ()
  "Apply font."
  (interactive)
  (let ((font "Iosevka-14"))
    (set-face-attribute 'default nil :font font)
    (set-face-attribute 'fixed-pitch nil :font font)
    (set-face-attribute 'fixed-pitch-serif nil :font font)
    (set-face-attribute 'variable-pitch nil :font font)))

(defmacro expand-dots-file (file)
  "Expand FILE from dotfiles `dots' directory."
  (let ((dir (expand-file-name
              "../dots/" (file-truename user-emacs-directory))))
    `(expand-file-name ,file ,dir)))

(defun stow-dotfiles (&optional prefix dir)
  "Stow dotfiles.
If PREFIX is non-nil, unstow dotfiles."
  (interactive "P\ni")
  (dolist (target (cdr (cdr (directory-files
                             (expand-dots-file (or dir "./")) t))))
    (if (or (file-regular-p target)
            (cl-dolist (parent '(".config/"))
              (let ((parent (expand-dots-file parent)))
                (when (and (file-in-directory-p target parent)
                           (not (file-equal-p target parent)))
                  (cl-return target)))))
        (let* ((rel-name (file-relative-name (expand-dots-file target)
                                             (expand-dots-file "./")))
               (link-name (expand-file-name rel-name "~/")))
          (when (file-exists-p link-name)
            (cond ((file-symlink-p link-name)
                   (delete-file link-name))
                  ((file-directory-p link-name)
                   (delete-directory link-name t))
                  (t (delete-file link-name)))
            (when prefix
              (message "Unlink: %s" link-name)))
          (unless prefix
            (make-symbolic-link target link-name)
            (message "Link: %s -> %s" target link-name)))
      (stow-dotfiles prefix target))))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'font-apply)
  (font-apply))

(require 'server)
(require 'package)
(require 'use-package)

(setq use-short-answers t
      use-dialog-box nil
      use-file-dialog nil
      inhibit-startup-screen t

      split-width-threshold nil
      split-height-threshold 0
      
      confirm-nonexistent-file-or-buffer nil
      Man-notify-method 'aggressive

      scroll-step 1
      scroll-margin 4
      scroll-preserve-screen-position t

      mode-line-percent-position '(6 "%q")
      eldoc-echo-area-use-multiline-p nil

      xref-file-name-display 'abs
      xref-prompt-for-identifier nil

      compile-command nil
      compilation-ask-about-save nil

      find-file-visit-truename t
      vc-follow-symlinks t
      auth-source-save-behavior nil

      c-default-style '((other . "user"))
      c-ts-mode-indent-offset 4

      dired-dwim-target t
      dired-listing-switches "-lah"
      dired-free-space 'separate
      dired-recursive-deletes 'always
      dired-auto-revert-buffer t

      display-line-numbers-width-start t
      display-line-numbers-grow-only t
      display-line-numbers-type 'relative

      backup-directory-alist `(("." . ,(expand-cache-file "backups/")))
      delete-old-versions t
      kept-old-versions 0
      kept-new-versions 5
      backup-by-copying t
      
      eglot-report-progress nil
      eglot-extend-to-xref t
      eglot-ignored-server-capabilities '(:documentHighlightProvider
                                          :codeLensProvider
                                          :documentOnTypeFormattingProvider
                                          :foldingRangeProvider
                                          :inlayHintProvider)

      package-user-dir (expand-cache-file "packages/")
      package-gnupghome-dir (expand-file-name "gnupg/" package-user-dir)
      use-package-always-ensure t
      use-package-always-defer (not (daemonp)))

(setq-default tab-width 4
              indent-tabs-mode nil)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents t)

(use-package no-littering
  :demand
  :preface
  (setq no-littering-var-directory (expand-cache-file))
  :custom
  (custom-file (expand-cache-file "custom.el"))
  (server-auth-dir (expand-cache-file "server/")))

(use-package gruber-darker-theme
  :init
  (load-theme 'gruber-darker t))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  (vertico-preselect 'no-prompt))

(use-package embark
  :bind
  (("C-," . embark-act)
   :map minibuffer-local-map
   ("C-." . embark-export)))

(use-package consult
  :bind
  (("C-x M-:" . consult-complex-command)
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x t b" . consult-buffer-other-tab)
   ("C-x p b" . consult-project-buffer)
   ("C-x r b" . consult-bookmark)
   ("C-s" . consult-line)
   ("M-y" . consult-yank-pop)
   ("M-g g" . consult-goto-line)
   ("C-c f" . consult-find)
   ("C-c g" . consult-grep)
   ("C-c G" . consult-git-grep)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))
  :custom
  (consult-line-start-from-top t)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-eglot
  :after consult
  :bind
  (("C-c l s" . eglot)
   :map eglot-mode-map
   ("C-c l i" . flymake-show-project-diagnostics)
   ("C-c l I" . flymake-show-buffer-diagnostics)
   ("C-c l S" . elgot-shutdown)
   ("C-c l f" . eglot-format)
   ("C-c l r" . eglot-rename)
   ("C-c l a" . eglot-code-actions)
   ("C-c l t" . eglot-find-typeDefinition)
   ("C-c l p" . consult-eglot-symbols)))

(use-package consult-eglot-embark
  :after (embark-consult consult-eglot)
  :init
  (consult-eglot-embark-mode 1))

(use-package company
  :bind
  (("M-<tab>" . company-complete)
   :map company-active-map
   ("<return>" . nil)
   ("RET" . nil)
   ("<tab>" . #'company-complete-selection)
   ("TAB" . #'company-complete-selection))
  :init
  (global-company-mode 1)
  :custom
  (company-idle-delay 0)
  (company-frontends '(company-pseudo-tooltip-frontend))
  (company-format-margin-function #'company-text-icons-margin)
  (company-tooltip-flip-when-above t)
  (company-tooltip-align-annotations t)
  (company-tooltip-scrollbar-width 0))

(use-package treesit-auto
  :demand
  :config 
  (defun treesit-install-all-grammars ()
    "Install all tree-sitter language grammmars."
    (interactive)
    (dolist (source (treesit-auto--build-treesit-source-alist))
      (let ((grammar (nth 0 source)))
        (unless (or (treesit-ready-p grammar t)
                    (member grammar '(janet latex markdown)))
          (message "Installing tree-sitter language grammer: %s" grammar)
          (let ((inhibit-message t)
                (outdir (nth 0 treesit-extra-load-path)))
            (apply #'treesit--install-language-grammar-1 outdir source))))))

  (add-to-list 'treesit-extra-load-path (expand-cache-file "treesit/"))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

(use-package vterm
  :bind
  (("C-c t" . vterm)
   :map vterm-mode-map
   ("C-j" . (lambda ()
              (interactive)
              (vterm-send "C-c"))))
  :config
  (setq vterm-timer-delay nil)
  (when-let* ((_ (string-match-p "bash" vterm-shell))
              (file (locate-user-emacs-file "vterm.sh"))
              (args (format "--init-file %s" file)))
    (setq vterm-shell (format "%s %s" vterm-shell args)))
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=Off")
  (vterm-always-compile-module t)
  (vterm-clear-scrollback-when-clearing t)
  (vterm-max-scrollback 50000))

(use-package magit)

(use-package move-text
  :bind
  (("M-p" . move-text-up)
   ("M-n" . move-text-down)))

(use-package switch-window
  :bind
  ("C-x o" . switch-window)
  :custom
  (switch-window-background t))

(use-package sudo-edit)

(use-package meson-mode)

(column-number-mode 1)
(global-display-line-numbers-mode 1)

(ffap-bindings)
(electric-pair-mode 1)
(editorconfig-mode 1)

(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)

(let ((file (locate-user-emacs-file "app-launcher.el")))
  (if (daemonp)
      (load-file file)
    (autoload #'app-launcher file t)))

(when (file-exists-p custom-file)
  (load-file custom-file))

(unless (or (daemonp)
            (server-running-p))
  (server-start))
