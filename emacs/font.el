(defgroup font nil
  "Font Management."
  :prefix "font-"
  :group 'emacs)

(defcustom font-name nil
  "Default font name."
  :type 'string)

(defcustom font-name-var (cond (rc/posix-p "DejaVu Sans")
                               (rc/mswin-p "Microsoft Sans Serif"))
  "Variable pitch font name."
  :type 'string)

(defcustom font-height nil
  "Default font height."
  :type 'natnum)

(defun font-load ()
  "Load font."
  (interactive)
  (when (and font-name font-name-var font-height)
    (let ((height (* font-height 10)))
      (set-face-attribute 'default nil :font font-name :height height)
      (set-face-attribute 'fixed-pitch nil :family font-name)
      (set-face-attribute 'fixed-pitch-serif nil :family font-name)
      (set-face-attribute 'variable-pitch nil :family font-name-var))))

(add-hook 'emacs-startup-hook #'font-load)

(defun font-nerds--ensure-font (font-name font-file)
  "Ensure that FONT-NAME with FONT-FILE is installed."
  (unless (member font-name (font-family-list))
    (message "Downloading font: %s" font-name)
    (let* ((target-dir (cond (rc/posix-p (rc/expand "~/.local/share/fonts/nerds/"))
                             (rc/mswin-p (rc/temp t))))
           (target-file (rc/expand font-file target-dir))
           (default-directory target-dir)
           (url "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/")
           (inhibit-message t)
           (message-log-max nil))
      (when rc/posix-p
        (make-directory target-dir t))
      (url-copy-file (concat url font-file) target-file t)
      (let* ((default-directory (dired-compress-file target-file))
             (items (cdr (cdr (directory-files default-directory t)))))
        (dolist (item items)
          (unless (string-match "\.ttf$" item)
            (if (file-directory-p item)
                (delete-directory item t)
              (delete-file item))))
        (delete-file target-file)
        (cond (rc/posix-p (rc/shell "fc-cache -f"))
              (rc/mswin-p (rc/script "fonts-install.ps1")))))))

(defun font-nerds--fetch-list ()
  "Fetch a list of downloadable nerd-fonts."
  (let ((fonts (make-hash-table :test 'equal))
        (max-name-len 0)
        (url "https://www.nerdfonts.com/font-downloads")
        (suf-NF '(Iosevka ZedMono))
        (bad '(Symbols)))
    (with-current-buffer (url-retrieve-synchronously url t)
      (let ((content (buffer-string))
            (last))
        (while (string-match "href=\"[^\"]+\\/\\([^/]+\\.zip\\)\"" content (match-end 0))
          (when-let*
              ((file (let ((match (match-string 1 content)))
                       (unless (string-equal last match)
                         (setq last match))))
               (name (progn
                       (string-match "invisible-text\"> \\([^<]+\\)" content (match-end 0))
                       (let* ((name (string-clean-whitespace (match-string 1 content)))
                              (suf (if (member (intern name) suf-NF) "NF" "Nerd Font")))
                         (format "%s %s" name suf))))
               (name-len (let ((len (length name)))
                           (if (> len max-name-len)
                               (setq max-name-len len)
                             len)))
               (info (progn
                       (string-match "Info:</strong> \\([^<]+\\)" content (match-end 0))
                       (let ((match (match-string 1 content)))
                         (setq match (string-clean-whitespace match))
                         (string-replace "\342\200\231" "'" match)))))
            (unless (member (intern name) bad)
              (puthash name (list :file file :info info :len name-len) fonts))))))
    `(:fonts ,fonts :max-name-len ,max-name-len)))

(defun font-nerds--completion-fun (str pred flag)
  "Completion function for nerd-fonts."
  (pcase flag
    ('metadata
     `(metadata
       (annotation-function
        . (lambda (cand)
            (let* ((info (plist-get (gethash cand fonts) :info))
                   (len (plist-get (gethash cand fonts) :len))
                   (ws (make-string (+ 2 (- max-name-len len)) ?\s))
                   (str (propertize info 'face 'completions-annotations)))
              (concat ws str))))))
    (_ (all-completions str fonts pred))))

(define-advice font-load (:before (&rest _) nerds)
  "Load nerd-font, if called interactively."
  (when (called-interactively-p)
    (let* ((font-list (font-nerds--fetch-list))
           (fonts (plist-get font-list :fonts))
           (max-name-len (plist-get font-list :max-name-len))
           (fun #'font-nerds--completion-fun)
           (name (completing-read "Load nerd-font: " fun nil t nil 'font-nerds))
           (height (round (read-number "Font height: " 13)))
           (file (plist-get (gethash name fonts) :file)))
      (font-nerds--ensure-font name file)
      (customize-save-variable 'font-name name)
      (customize-save-variable 'font-height height)
      (unless font-name-var
        (customize-save-variable 'font-name-var name)))))

(define-minor-mode font-nerds-mode
  "If non-nil, usage of nerd-fonts is enabled."
  :init-value t
  :global t
  :lighter nil
  (if font-nerds-mode
      (advice-add 'font-load :before #'font-load@nerds)
    (advice-remove 'font-load #'font-load@nerds)))
