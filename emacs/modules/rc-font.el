(defgroup font nil
  "Font Management"
  :prefix "font-"
  :group 'emacs)

(defcustom font-name "Hack Nerd Font"
  "Font name."
  :type 'string)

(defcustom font-height 13
  "Font height."
  :type 'string)

(defcustom font-after-load-hook nil
  "Hooks to run after font gets loaded."
  :type 'hook)

(defvar font--list nil
  "Hashmap of available nerd-fonts.")

(defvar font--curr-max-name-len 0
  "Current maximum font name length")

(defun font-height (&optional offset)
  "Get font height with OFFSET.
Values are relative and normalized for size 13."
  (+ rc-font-height (floor (* (or offset 0) (/ font-height 13)))))

(defun font-ensure (font)
  "Ensure nerd-font FONT is installed."
  (interactive)
  (let* ((asset-name (rc-fetch-font-asset-name rc-font))
	 (default-directory
          (rc-expand (concat "~/.local/share/fonts/" asset-name "/")))
	 (font-archive (concat asset-name ".tar.xz"))
	 (link (concat
                "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/"
                font-archive)))
    (make-directory default-directory t)
    (rc-shell (rc-join "curl -sLO" link "&&"
                       "tar xJf" font-archive "&&"
                       "fc-cache -f &&"
                       "rm" font-archive)
      (message "Extracted archive %s to %s" font-archive default-directory))))

(defun font-fetch-list ()
  "Fetch list of available nerd-fonts."
  (with-current-buffer
      (url-retrieve-synchronously "https://www.nerdfonts.com/font-downloads")
    (let ((content (buffer-string))
          (matches (make-hash-table :test 'equal)))
      (while (string-match "/\\([^/]+\\)\\.svg" content (match-end 0))
        (let* ((name (match-string 1 content))
               (len (length name))
               (file (progn
                       (string-match
                        "invisible-text\"> \\([^<]+\\)" content (match-end 0))
                       (string-clean-whitespace (match-string 1 content))))
               (info (progn
                       (string-match
                        "Info:</strong> \\([^<]+\\)" content (match-end 0))
                       (string-replace
                        "\342\200\231" "‘"
                        (string-clean-whitespace (match-string 1 content))))))
          (puthash name (list :file file :info info :len len) matches)
          (when (> len font--curr-max-name-len)
            (setq font--curr-max-name-len length))))
      matches)))

(defun font-load--annotation (cand)
  "Annotation function for `font-load' font prompt."
  (let ((info (plist-get (gethash cand font--list) :info))
        (len (plist-get (gethash cand font--list) :len)))
    (if info
        (concat (make-string (+ 2 (if icomplete-mode
                                      0
                                    (- font--curr-max-name-len len)))
                             ?\s)
                (propertize info 'face 'completions-annotations))
      "")))

(defun font-load (&optional prefix)
  "Load font.
If PREFIX is non-nil, do not run hooks `font-after-load-hook'.

If this command is called interactively, prompt for the font."
  (interactive "P")
  (let ((font (rc-cookie :font-name))
        (height (rc-cookie :font-height)))
    
    (when (or (interactive-p) (not (and font height)))
      (progn
        (unless font--list
          (setq font--list (font-fetch-list)))
        (let ((name
               (completing-read
                "Load font: "
	        (lambda (str pred flag)
                  (pcase flag
                    ('metadata
                     `(metadata
                       (annotation-function . font-load--annotation)))
                    (_ (all-completions str font--list pred))))
	        nil t nil 'font-load-name nil nil))
              (height
               (round
                (read-number "Font height: " nil 'font-load-height)))))))

    (let ((height (* height 10)))
      (set-face-attribute 'default nil :font font :height height)
      (set-face-attribute 'fixed-pitch nil :family font  :height height)
      (set-face-attribute 'fixed-pitch-serif nil :family font :height height)
      (set-face-attribute 'variable-pitch nil :family font :height height))

    (rc-cookie :font-name font)
    (rc-cookie :font-height height))
  
  (unless prefix
    (run-hooks 'font-after-load-hook)))

(let ((font font-name)
      (height (* font-height 10)))
      (set-face-attribute 'default nil :font font :height height)
      (set-face-attribute 'fixed-pitch nil :family font  :height height)
      (set-face-attribute 'fixed-pitch-serif nil :family font :height height)
      (set-face-attribute 'variable-pitch nil :family font :height height))

(provide 'rc-font)
