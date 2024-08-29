(defgroup font nil
  "Font Management."
  :prefix "font-"
  :group 'emacs)

(defcustom font-name (cond ((rc-windows-p) "Cascadia Code"))
  "Default font name."
  :type 'string)

(defcustom font-name-var (cond ((rc-posix-p) "DejaVu Sans")
                               ((rc-windows-p) "Microsoft Sans Serif"))
  "Variable pitch font name."
  :type 'string)

(defcustom font-height (cond ((rc-windows-p) 12))
  "Default font height."
  :type 'natnum)

(defcustom font-nerds-ignore-fonts '("Symbols")
  "List of nerd-fonts to ignore."
  :type '(repeat string))

(defvar font-nerds--font-list (make-hash-table :test 'equal)
  "Hashmap of available nerd-fonts.")

(defvar font-nerds--curr-max-name-len 0
  "Current maximum nerd-font name length.")

(define-minor-mode font-nerds-mode
  "Toggle usage of nerd-fonts."
  :init-value (rc-posix-p)
  :global t
  :lighter nil
  (unless (rc-posix-p)
    (error "Can't use `font-nerds-mode' on non-posix systems")))

(defun font-nerds-fetch-list ()
  "Fetch list of available nerd-fonts into `font-nerds--font-list'."
  (when (= (hash-table-count font-nerds--font-list) 0)
    (clrhash font-nerds--font-list)
    (with-current-buffer
        (url-retrieve-synchronously "https://www.nerdfonts.com/font-downloads")
      (let ((content (buffer-string))
            (last nil))
        (while (string-match "href=\"[^\"]+\\/\\([^/]+\\.zip\\)\"" content (match-end 0))
          (when-let*
              ((file (let ((match (match-string 1 content)))
                       (unless (string-equal last match)
                         (setq last match))))
               (name (progn
                       (string-match "invisible-text\"> \\([^<]+\\)" content (match-end 0))
                       (string-clean-whitespace (match-string 1 content))))
               (len (length name))
               (info (progn
                       (string-match "Info:</strong> \\([^<]+\\)" content (match-end 0))
                       (string-replace
                        "\342\200\231" "'"
                        (string-clean-whitespace (match-string 1 content))))))
            (unless (member name font-nerds-ignore-fonts)
              (puthash
               (rc-join name "Nerd Font")
               (list :file file :info info :len len)
               font-nerds--font-list)
              (when (> len font-nerds--curr-max-name-len)
                (setq font-nerds--curr-max-name-len len)))))))))

(defun font-nerds-ensure-font ()
  "Ensure `font-name' for posix systems."
  (let* ((file (plist-get (gethash font-name font-nerds--font-list) :file))
	     (default-directory
          (rc-expand (concat (file-name-base file) "/") "~/.local/share/fonts/rc/"))
	     (link
          (concat
           "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/" file)))
    (make-directory default-directory t)
    (cond
     ((rc-posix-p)
      (rc-shell (rc-join
                 "fc-list : file family | grep"
                 default-directory "| grep -q" (prin1-to-string font-name))
        nil
        (progn
          (message "Downloading nerd-font %s" (prin1-to-string font-name))
          (rc-shell (rc-join "curl -sLO" link "&&"
                             "unzip -qo" file "&&"
                             "fc-cache -f &&"
                             "rm" file)
            (message "Extracted %s to %s" file default-directory)))))
     (t (message
         "Assuming nerd-font %s is installed" (prin1-to-string font-name))))))

(defun font-nerds-query-font ()
  "Query for the nerd-font and their height."
  (let ((name
         (completing-read
          "Default nerd-font: "
	      (lambda (str pred flag)
            (pcase flag
              ('metadata
               `(metadata
                 (annotation-function
                  . (lambda (cand)
                      (let ((info
                             (plist-get (gethash cand font-nerds--font-list) :info))
                            (len
                             (plist-get (gethash cand font-nerds--font-list) :len)))
                        (concat
                         (make-string
                          (+ 2 (if icomplete-mode
                                   0 (- font-nerds--curr-max-name-len len)))
                          ?\s)
                         (propertize info 'face 'completions-annotations)))))))
              (_ (all-completions str font-nerds--font-list pred))))
	      nil nil nil t nil nil))
        (height
         (round
          (read-number "Default nerd-font height: " nil t))))
    (customize-save-variable 'font-name name)
    (customize-save-variable 'font-height height)
    (unless font-name-var
      (customize-save-variable 'font-name-var font-name))))

(defun font-load (&optional prefix)
  "Load current fonts.

If `font-nerds-mode' is active, handle nerd-font loading.
If `font-name' or `font-height' is nil, query for them.
If optional PREFIX is non-nil, query for them anyways."
  (interactive "P")
  (when (and font-nerds-mode
             (or prefix
                 (not (and font-name font-name-var font-height))))
    (font-nerds-fetch-list)
    (font-nerds-query-font)
    (font-nerds-ensure-font))
  (set-face-attribute 'default nil :font font-name :height (* font-height 10))
  (set-face-attribute 'fixed-pitch nil :family font-name)
  (set-face-attribute 'fixed-pitch-serif nil :family font-name)
  (set-face-attribute 'variable-pitch nil :family font-name-var))

(add-hook 'elpaca-after-init-hook 'font-load)

(provide 'font)
