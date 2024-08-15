(defgroup font nil
  "Font Management"
  :prefix "font-"
  :group 'emacs)

(setq query-all-font-backends t)

(defcustom font-name nil
  "Font name."
  :type 'string)

(defcustom font-height nil
  "Font height."
  :type 'natnum)

(defvar font--list nil
  "Hashmap of available nerd-fonts.")

(defvar font--curr-max-name-len 0
  "Current maximum font name length")

(defun font-height (&optional offset)
  "Get font height with OFFSET.
Values are relative to and normalized for `font-ref-height'."
  (+ font-height (floor (* (or offset 0) (/ font-height 13)))))

(defun font-ensure (font)
  "Ensure nerd-font FONT is installed."
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

(defun font-fetch-list ()
  "Fetch list of available nerd-fonts."
  (with-current-buffer
      (url-retrieve-synchronously "https://www.nerdfonts.com/font-downloads")
    (let ((content (buffer-string))
          (matches (make-hash-table :test 'equal))
          (file nil)
          (name nil)
          (len nil))
      (while (string-match "href=\"[^\"]+\\/\\([^/]+\\.zip\\)\"" content (match-end 0))
        (unless (string-equal file (match-string 1 content))
          (setq file (match-string 1 content)
                name (progn
                       (string-match "invisible-text\"> \\([^<]+\\)" content (match-end 0))
                       (rc-join
                        (string-clean-whitespace (match-string 1 content)) "Nerd Font"))
                len (length name)
                info (progn
                       (string-match "Info:</strong> \\([^<]+\\)" content (match-end 0))
                       (string-replace
                        "\342\200\231" "'"
                        (string-clean-whitespace (match-string 1 content)))))
          (print `(,file ,name ,info))
          (puthash name (list :file file :info info :len len) matches)
          (when (> len font--curr-max-name-len)
            (setq font--curr-max-name-len len))))
      matches)))

(defun font-load--annotation (cand)
  "Annotation function for `font-load' font prompt."
  (let ((info (plist-get (gethash cand font--list) :info))
        (len (plist-get (gethash cand font--list) :len)))
    (concat
     (make-string (+ 2 (if icomplete-mode 0 (- font--curr-max-name-len len))) ?\s)
     (propertize info 'face 'completions-annotations))))

(defun font-load ()
  "Load font `'font-name' with height `font-height'.
If this command is called interactively, prompt for the values."
  (interactive)
  (when (or (interactive-p)
            (not (and font-name font-height)))
    (unless font--list
      (setq font--list (font-fetch-list)))
    (customize-save-variable
     'font-name (completing-read
                 "Load font: "
	             (lambda (str pred flag)
                   (pcase flag
                     ('metadata
                      `(metadata
                        (annotation-function . font-load--annotation)))
                     (_ (all-completions str font--list pred))))
	             nil t nil 'font-load-name nil nil))
    (let ((height (round (read-number "Font height: " nil 'font-load-height))))
      (customize-save-variable 'font-height (if (>= height 0) height (font-height))))
    (font-ensure font-name))
  (let ((height (* font-height 10)))
    (set-face-attribute 'default nil :font font-name :height height)
    (set-face-attribute 'fixed-pitch nil :family font-name  :height height)
    (set-face-attribute 'fixed-pitch-serif nil :family font-name :height height)
    (set-face-attribute 'variable-pitch nil :family font-name :height height)))

(add-hook 'after-init-hook 'font-load)

(provide 'fonts)
