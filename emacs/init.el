(setq use-short-answers t
      suggest-key-bindings nil
      vc-follow-symlinks t)

(setq warning-minimum-level :emergency
      ad-redefinition-action 'accept)

(setq auth-source-save-behavior nil)

(setq dired-listing-switches "-lah"
      dired-free-space 'separate
      dired-recursive-deletes 'always
      dired-dwim-target t
      dired-auto-revert-buffer t
      dired-clean-confirm-killing-deleted-buffers nil)

(with-eval-after-load 'dired
  (require 'dired-x)
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1))))

(mapc
 (lambda (file)
   (catch 'return
     (load (file-truename file) t t)))
 (directory-files (locate-user-emacs-file "lisp/") t ".el$"))

(setq custom-file (rc-cache "custom.el"))
(load custom-file t t)

(define-advice custom-save-all (:around (fun &rest args) silent)
  "Save all custom variables silently."
  (let ((save-silently t))
    (apply fun args)))
