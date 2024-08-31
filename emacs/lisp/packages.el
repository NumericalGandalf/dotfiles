(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (rc-cache "elpaca/"))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order
  '(elpaca :repo "https://github.com/progfolio/elpaca.git"
           :ref nil :depth 1
           :files (:defaults "elpaca-test.el" (:exclude "extensions"))
           :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop
                   (apply #'call-process
                          `("git" nil ,buffer t "clone"
                            ,@(when-let ((depth (plist-get order :depth)))
                                (list
                                 (format "--depth=%d" depth)
                                 "--no-single-branch"))
                            ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process
                          "git" nil buffer t "checkout"
                          (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process
                          emacs nil buffer nil
                          "-Q" "-L" "." "--batch" "--eval"
                          "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(when (rc-windows-p)
  (setq elpaca-queue-limit 20)
  (elpaca-no-symlink-mode))

(dolist (package '(transient))
  (setq elpaca-ignored-dependencies
        (remove package elpaca-ignored-dependencies)))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(setq use-package-always-ensure t
      use-package-hook-name-suffix nil)

(when init-file-debug
  (setq use-package-compute-statistics t))

(use-package general)

(use-package no-littering
  :init
  (setq no-littering-etc-directory (rc-expand)
        no-littering-var-directory (rc-cache)))

(elpaca-wait)

(provide 'packages)
