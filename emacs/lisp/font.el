(defgroup font nil
  "Font Management"
  :prefix "font-"
  :group 'emacs)

(defcustom font-name-def nil
  "Default font name."
  :type 'string)

(defcustom font-name-var nil
  "Variable pitch font name."
  :type 'string)

(defcustom font-height-def nil
  "Default font height."
  :type 'natnum)

(defcustom font-height-var nil
  "Variable pitch font height."
  :type 'natnum)

(defcustom font-nerds-ignore-fonts '("Symbols")
  "List of nerd-fonts to ignore."
  :type '(repeat string))

(defvar font-nerds--font-list (make-hash-table :test 'equal)
  "Hashmap of available nerd-fonts.")

(defvar font-nerds--curr-max-name-len 0
  "Current maximum nerd-font name length.")

(defun font-load ()
  "Load current fonts."
  (interactive)
  (let* ((font-name-var (or font-name-var font-name-def))
         (height-def (* font-height-def 10))
         (height-var (if font-height-var
                         (* font-height-var 10)
                       height-def)))
    (set-face-attribute
     'default nil :font font-name-def :height height-def)
    (set-face-attribute
     'fixed-pitch nil :family font-name-def :height height-def)
    (set-face-attribute
     'fixed-pitch-serif nil :family font-name-def :height height-def)
    (set-face-attribute
     'variable-pitch nil :family font-name-var :height height-var)))

(add-hook 'elpaca-after-init-hook 'font-load)

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

(defun font-nerds-query-font ()
  "Query for fonts and their heights."
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
	      nil nil nil 'font-nerds-query-name nil nil))
        (height
         (round
          (read-number "Default nerd-font height: " nil 'font-nerds-query-height))))
    (customize-save-variable 'font-name-def name)
    (customize-save-variable 'font-height-def height)))

(defun font-nerds-ensure-font ()
  "Ensure `font-name-def' for posix systems."
  (let* ((file (plist-get (gethash font-name-def font-nerds--font-list) :file))
	     (default-directory
          (rc-expand (concat (file-name-base file) "/") "~/.local/share/fonts/rc/"))
	     (link
          (concat
           "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/" file)))
    (make-directory default-directory t)
    (cond
     (rc-posix-p
      (rc-shell (rc-join
                 "fc-list : file family | grep"
                 default-directory "| grep -q" (prin1-to-string font-name-def))
        nil
        (progn
          (message "Downloading nerd-font %s" (prin1-to-string font-name-def))
          (rc-shell (rc-join "curl -sLO" link "&&"
                             "unzip -qo" file "&&"
                             "fc-cache -f &&"
                             "rm" file)
            (message "Extracted %s to %s" file default-directory)))))
     (t (message
         "Assuming nerd-font %s is installed" (prin1-to-string font-name-def))))))

(defun font-load@nerds (fun &rest args)
  "Include nerd-fonts in font loading."
  (unless (and (not current-prefix-arg)
               font-name-def font-height-def)
    (font-nerds-fetch-list)
    (font-nerds-query-font)
    (font-nerds-ensure-font))
  (apply fun args))

(define-minor-mode font-nerds-mode
  "Toggle usage of nerd-fonts."
  :global t
  :lighter nil
  (unless rc-posix-p
    (error
     "Can't use `font-nerds-mode' on system %s"
     (prin1-to-string (symbol-name system-type))))
  (if font-nerds-mode
      (advice-add 'font-load :around 'font-load@nerds)
    (advice-remove 'font-load 'font-load@nerds)))

(provide 'font)
