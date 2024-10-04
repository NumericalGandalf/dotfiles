(defgroup nerd-fonts nil
  "Nerd-fonts management."
  :prefix "nerd-fonts-"
  :group 'emacs)

(defcustom nerd-fonts-name nil
  "Nerd-font name."
  :type 'string)

(defcustom nerd-fonts-height nil
  "Nerd-font height."
  :type 'natnum)

(defun nerd-fonts--ensure-font (font-name font-file)
  "Ensure FONT-NAME with FONT-FILE is installed."
  (let* ((target-dir (rc/temp t))
         (target-file (rc/expand font-file target-dir))
         (default-directory target-dir)
         (url "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/"))
    (url-copy-file (concat url font-file) target-file t)
    (let* ((default-directory (dired-compress-file target-file)))
      (rc/script (cond (rc/posix-p "fonts-install.sh")
                       (rc/mswin-p "fonts-install.ps1"))))))

(defun nerd-fonts--fetch-list ()
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

(defun nerd-fonts--completion-fun (str pred flag)
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

(defun nerd-fonts-load ()
  "Load font `nerd-fonts-name' with height `nerd-fonts-height'."
  (when (or (not (and nerd-fonts-name nerd-fonts-height))
            (called-interactively-p))
    (let* ((font-list (nerd-fonts--fetch-list))
           (fonts (plist-get font-list :fonts))
           (max-name-len (plist-get font-list :max-name-len))
           (fun #'nerd-fonts--completion-fun)
           (name (completing-read "Load nerd-font: " fun nil t nil 'nerd-fonts))
           (height (round (read-number "Nerd-font height: " 13)))
           (file (plist-get (gethash name fonts) :file))
           (save-silently t))
      (unless (member name (font-family-list))
        (nerd-fonts--ensure-font name file))
      (customize-save-variable 'nerd-fonts-name name)
      (customize-save-variable 'nerd-fonts-height height)))
  (let ((height (* nerd-fonts-height 10)))
    (set-face-attribute 'default nil :font nerd-fonts-name :height height)))

(add-hook 'after-init-hook #'nerd-fonts-load)
