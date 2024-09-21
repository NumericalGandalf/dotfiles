(defgroup font nil
  "Font Management."
  :prefix "font-"
  :group 'emacs)

(defcustom font-name (cond (rc/mswin-p "Cascadia Code"))
  "Default font name."
  :type 'string)

(defcustom font-name-var (cond (rc/posix-p "DejaVu Sans")
                               (rc/mswin-p "Microsoft Sans Serif"))
  "Variable pitch font name."
  :type 'string)

(defcustom font-height (cond (rc/mswin-p 12))
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

(defun font-nerds--ensure-font (font-name file)
  "Ensure that FONT-NAME with FILE is installed."
  (let* ((font-name (prin1-to-string font-name))
         (url "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/")
         (url1 (concat url file))
         (dir (rc/expand "~/.local/share/fonts/nerds/"))
         (file (rc/expand file dir))
         (cmd (format "fc-list : file family | grep %s | grep -q %s" dir font-name)))
    (make-directory dir t)
    (unless (= (call-process-shell-command cmd) 0)
      (progn
        (url-copy-file url1 file t)
        (dired-compress-file file)
        (call-process-shell-command "fc-cache -f")
        (delete-file (rc/expand file))))))

(defun font-nerds--fetch-list ()
  "Fetch a list of downloadable nerd-fonts.

Return a plist with :font-list and :max-name-len."
  (let ((fonts (make-hash-table :test 'equal))
        (max-name-len 0)
        (url "https://www.nerdfonts.com/font-downloads"))
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
                       (string-clean-whitespace (match-string 1 content))))
               (name-len (let ((len (length name)))
                           (when (> len max-name-len)
                             (setq max-name-len len))
                           len))
               (info (progn
                       (string-match "Info:</strong> \\([^<]+\\)" content (match-end 0))
                       (let ((match (match-string 1 content)))
                         (setq match (string-clean-whitespace match))
                         (string-replace "\342\200\231" "'" match)))))
            (unless (member name '("Symbols"))
              (puthash
               (format "%s Nerd Font" name)
               (list :file file :info info :len name-len)
               fonts))))))
    `(:fonts ,fonts :max-name-len ,max-name-len)))

(defun font-nerds--query-font ()
  "Query for a nerd-font and a height."
  (let* ((prompt1 "Load nerd-font: ")
         (prompt2 "Font height: ")
         (font-list (font-nerds--fetch-list))
         (fonts (plist-get font-list :fonts))
         (max-name-len (plist-get font-list :max-name-len))
         (fun1 (lambda (cand)
                 (let* ((info (plist-get (gethash cand fonts) :info))
                        (len (plist-get (gethash cand fonts) :len))
                        (ws (make-string (+ 2 (- max-name-len len)) ?\s))
                        (str (propertize info 'face 'completions-annotations)))
                   (concat ws str))))
         (fun (lambda (str pred flag)
                (pcase flag
                  ('metadata `(metadata (annotation-function . ,fun1)))
                  (_ (all-completions str fonts pred)))))
         (name (completing-read prompt1 fun nil nil nil t nil nil))
         (height (round (read-number prompt2 nil t)))
         (file (plist-get (gethash name fonts) :file)))
    `(:name ,name :height ,height :file ,file)))

(define-advice font-load (:before (&rest _) nerds)
  "Load nerd-font, if called interactively."
  (when (called-interactively-p)
    (let* ((font (font-nerds--query-font))
           (name (plist-get font :name)))
      (font-nerds--ensure-font name (plist-get font :file))
      (customize-save-variable 'font-name name)
      (customize-save-variable 'font-height (plist-get font :height))
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

(provide 'font)
