(defgroup font nil
  "Font management."
  :prefix "font/")

(defvar font/name nil
  "Font name.")

(defvar font/height nil
  "Font height.")

(defvar font/fetched-list nil
  "Hashmap of available fonts.")

(defvar font/fetched-last 0
  "Last time `font/fetched-list' has been fetched.")

(defvar font/need-apply nil
  "Whether `font/apply' needs to be run.")

(defun font/fetch-list ()
  "Fetch a list of available fonts."
  (let ((fonts (make-hash-table :test 'equal))
        (max-name-len 0))
    (with-current-buffer (url-retrieve-synchronously "https://www.nerdfonts.com/font-downloads" t)
      (let ((content (buffer-string))
            (last))
        (while (string-match "href=\"[^\"]+\\/\\([^/]+\\.zip\\)\"" content (match-end 0))
          (when-let*
              ((file (let ((match (match-string 1 content)))
                       (unless (string-equal last match)
                         (setq last match))))
               (name (progn
                       (string-match "invisible-text\"> \\([^<]+\\)" content (match-end 0))
                       (format "%s Nerd Font" (string-clean-whitespace (match-string 1 content)))))
               (name-len (let ((len (length name)))
                           (if (> len max-name-len)
                               (setq max-name-len len)
                             len)))
               (info (progn
                       (string-match "Info:</strong> \\([^<]+\\)" content (match-end 0))
                       (let ((match (match-string 1 content)))
                         (setq match (string-clean-whitespace match))
                         (string-replace "\342\200\231" "'" match)))))
            (puthash name `(:file ,file :info ,info :len ,name-len) fonts)))))
    `(:fonts ,fonts :max-name-len ,max-name-len)))

(defun font/ensure (font)
  "Ensure that FONT is installed."
  (unless (member font (font-family-list))
    ;; Download the font to fc-cache directory.
    (let* ((file (plist-get (gethash font (plist-get font/fetched-list :fonts)) :file))
           (target-dir (expand-file-name "~/.local/share/fonts/"))
           (default-directory target-dir)
           (target-file (expand-file-name file target-dir))
           (url (concat "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/" file)))
      (mkdir target-dir t)
      (message "Download font: %s" url)
      (let ((inhibit-message t)
            (message-log-max nil))
        (url-copy-file url target-file t))
      (dired-compress-file target-file)
      (delete-file target-file)
      (call-process-shell-command "fc-cache -rv"))))

(defun font/write-values (name height)
  "Write `font/name' and `font/height' values."
  (let ((file (rc/expand-cache-lisp-file "font-values.el")))
    (with-temp-file file
      (erase-buffer)
      (insert (format "(setq %s \"%s\")\n" (symbol-name 'font/name) name))
      (insert (format "(setq %s %s)" (symbol-name 'font/height) height)))
    (load-file file)))

(defun font/query ()
  "Query `font/name' and `font/height' values."
  (let ((stamp (time-to-seconds)))
    (when (> (- stamp font/fetched-last) (* 5 60 60))
      ;; Frequent fetching is unnecessary.
      (setq font/fetched-list (font/fetch-list)
            font/fetched-last stamp)))
  (let* ((fonts (plist-get font/fetched-list :fonts))
         (max-name-len (plist-get font/fetched-list :max-name-len))
         (fun-1 (lambda (cand)
                  (let* ((info (plist-get (gethash cand fonts) :info))
                         (len (plist-get (gethash cand fonts) :len))
                         (ws (make-string (+ 2 (- max-name-len len)) ?\s))
                         (str (propertize info 'face 'completions-annotations)))
                    (concat ws str))))
         (fun (lambda (str pred flag)
                (pcase flag
                  ('metadata `(metadata (annotation-function . ,fun-1)))
                  (_ (all-completions str fonts pred)))))
         (name (completing-read "Load font: " fun nil t))
         (height (round (read-number "Font height: " 13))))
    `(,name . ,height)))

;;;###autoload
(defun font/load (&optional prefix)
  "Load font.
If PREFIX is non-nil, force to query the font."
  (interactive "P")
  (if (or prefix
          ;; Also query if the values are nil.
          (not (and font/name font/height))
          (not (member font/name (font-family-list))))
      (let ((font (font/query)))
        (font/ensure (car font))
        (font/write-values (car font) (cdr font)))
    (let ((height (* font/height 10)))
      (set-face-attribute 'default nil :font font/name :height height)
      (set-face-attribute 'fixed-pitch nil :font font/name :height height)
      (set-face-attribute 'fixed-pitch-serif nil :font font/name :height height)
      (set-face-attribute 'variable-pitch nil :font font/name :height height))))

;;;###autoload
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'font/load)
  (if after-init-time
      (font/load)
    (add-hook 'emacs-startup-hook #'font/load)))

(provide 'font)
