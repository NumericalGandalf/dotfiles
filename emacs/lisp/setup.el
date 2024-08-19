(defcustom elpaca-auto-update-interval 7
  "Interval in days for package auto upgrading."
  :type 'natnum)

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

(when rc-windows-p
  (elpaca-no-symlink-mode))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-hook-name-suffix nil
        use-package-compute-statistics t))

(defun elpaca-auto-update (&optional prefix)
  "Update packages if `elpaca-auto-update-interval' has passed.
If optional PREFIX is non-nil, force update."
  (interactive "P")
  (let ((file (rc-expand "update" elpaca-directory))
        (day (time-to-days (current-time))))
    (when (or prefix
              (not (file-exists-p file))
              (>= day (string-to-number
                       (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string)))))
      (elpaca-pull-all)
      (rc-file file
        (insert (int-to-string (+ day elpaca-auto-update-interval)))))))

(add-hook 'elpaca-after-init-hook 'elpaca-auto-update)

(use-package no-littering
  :demand
  :init
  (setq no-littering-etc-directory (rc-expand)
        no-littering-var-directory (rc-cache)))

(provide 'setup)
