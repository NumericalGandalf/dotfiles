(defgroup font nil
  "Font Management"
  :prefix "font-"
  :group 'emacs)

(defcustom font-name nil
  "Font name."
  :type 'string)

(defcustom font-height nil
  "Font height."
  :type 'natnum)

(defcustom font-ref-height 13
  "Font refrence height."
  :type 'natnum)

(defcustom font-ignore '("Symbols")
  "List of nerd fonts to ignore."
  :type '(repeat string))

(defvar font--list (make-hash-table :test 'equal)
  "Hashmap of available nerd-fonts.")

(defvar font--curr-max-name-len 0
  "Current maximum font name length.")

(defun font-height (offset)
  "Get font height with OFFSET.
OFFSET is relative to and normalized for `font-ref-height'."
  (+ font-height
     (floor (* (or offset 0)
               (/ font-height font-ref-height)))))

(defun font-ensure (font)
  "Ensure FONT for posix systems."
  (let* ((file (plist-get (gethash font font--list) :file))
	     (default-directory
          (rc-expand (concat (file-name-base file) "/") "~/.local/share/fonts/rc/"))
	     (link
          (concat
           "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/" file)))
    (make-directory default-directory t)
    (cond (rc-posix-p
           (rc-shell (rc-join
                      "fc-list : file family | grep"
                      default-directory "| grep -q" (prin1-to-string font))
             nil
             (progn
               (message "Downloading font %s" (prin1-to-string font))
               (rc-shell (rc-join "curl -sLO" link "&&"
                                  "unzip -qo" file "&&"
                                  "fc-cache -f &&"
                                  "rm" file)
                 (message "Extracted %s to %s" file default-directory)))))
          (t (message "Assuming font %s is installed" (prin1-to-string font))))))

(defun font-fetch-list (&optional force)
  "Fetch list of available nerd-fonts into `font--list'.
If optional FORCE is non-nil, fetch even if `font--list' is not empty."
  (when (or force
            (= (hash-table-count font--list) 0))
    (clrhash font--list)
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
            (unless (member name font-ignore)
              (puthash
               (rc-join name "Nerd Font")
               (list :file file :info info :len len)
               font--list)
              (when (> len font--curr-max-name-len)
                (setq font--curr-max-name-len len)))))))))

(defun font-query ()
  "Query for and set `font-name' and `font-height'."
  (font-fetch-list)
  (let ((name (completing-read
               "Load font: "
	           (lambda (str pred flag)
                 (pcase flag
                   ('metadata
                    `(metadata
                      (annotation-function
                       . (lambda (cand)
                           (let ((info (plist-get (gethash cand font--list) :info))
                                 (len (plist-get (gethash cand font--list) :len)))
                             (concat
                              (make-string
                               (+ 2
                                  (if icomplete-mode
                                      0 (- font--curr-max-name-len len)))
                               ?\s)
                              (propertize info 'face 'completions-annotations)))))))
                   (_ (all-completions str font--list pred))))
	           nil t nil 'font-query-name nil nil))
        (height (round (read-number "Font height: " nil 'font-query-height))))
    (customize-save-variable 'font-name name)
    (customize-save-variable 'font-height (if (>= height 0)
                                              height font-ref-height))))

(defun font-load (&optional prefix)
  "Load `font-name' and `font-height'.
If any of these are nil, run `font-query' and `font-ensure' first.
If optional PREFIX is non-nil, query anyways."
  (interactive "P")
  (if (and (not prefix) font-name font-height)
      (let ((height (* font-height 10)))
        (set-face-attribute 'default nil :font font-name :height height)
        (set-face-attribute 'fixed-pitch nil :family font-name  :height height)
        (set-face-attribute 'fixed-pitch-serif nil :family font-name :height height)
        (set-face-attribute 'variable-pitch nil :family font-name :height height))
    (progn
      (font-query)
      (font-ensure font-name)
      (font-load))))

(add-hook 'after-init-hook 'font-load)

(provide 'fonts)
