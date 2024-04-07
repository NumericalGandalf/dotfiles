;;; zenburn-theme.el --- A low contrast color theme for Emacs.

;;; Code:

(deftheme zenburn "The Zenburn color theme.")

(defgroup zenburn-theme nil
  "Zenburn theme."
  :group 'faces
  :prefix "zenburn-"
  :link `(url-link :tag "GitHub" "http://github.com/bbatsov/zenburn-emacs")
  :tag "Zenburn theme")

;;;###autoload
(defcustom zenburn-override-colors-alist `()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist."
  :group 'zenburn-theme
  :type `(alist
          :key-type (string :tag "Name")
          :value-type (string :tag " Hex")))

(defvar zenburn-use-variable-pitch nil
  "When non-nil, use variable pitch face for some headings and titles.")

(defvar zenburn-scale-org-headlines nil
  "Whether `org-mode' headlines should be scaled.")

(defvar zenburn-scale-outline-headlines nil
  "Whether `outline-mode' headlines should be scaled.")

(defcustom zenburn-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'zenburn-theme
  :package-version `(zenburn . "2.6"))

(defcustom zenburn-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'zenburn-theme
  :package-version `(zenburn . "2.6"))

(defcustom zenburn-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'zenburn-theme
  :package-version `(zenburn . "2.6"))

(defcustom zenburn-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'zenburn-theme
  :package-version `(zenburn . "2.6"))

(defcustom zenburn-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'zenburn-theme
  :package-version `(zenburn . "2.6"))

;;; Color Palette

(defvar zenburn-default-colors-alist
  `(("zenburn-fg-1"     . "#656555")
    ("zenburn-fg-05"    . "#989890")
    ("zenburn-fg"       . "#DCDCCC")
    ("zenburn-fg+1"     . "#FFFFEF")
    ("zenburn-fg+2"     . "#FFFFFD")
    ("zenburn-bg-2"     . "#000000")
    ("zenburn-bg-1"     . "#2B2B2B")
    ("zenburn-bg-08"    . "#303030")
    ("zenburn-bg-05"    . "#383838")
    ("zenburn-bg"       . "#3F3F3F")
    ("zenburn-bg+05"    . "#494949")
    ("zenburn-bg+1"     . "#4F4F4F")
    ("zenburn-bg+2"     . "#5F5F5F")
    ("zenburn-bg+3"     . "#6F6F6F")
    ("zenburn-red-6"    . "#6C3333")
    ("zenburn-red-5"    . "#7C4343")
    ("zenburn-red-4"    . "#8C5353")
    ("zenburn-red-3"    . "#9C6363")
    ("zenburn-red-2"    . "#AC7373")
    ("zenburn-red-1"    . "#BC8383")
    ("zenburn-red"      . "#CC9393")
    ("zenburn-red+1"    . "#DCA3A3")
    ("zenburn-red+2"    . "#ECB3B3")
    ("zenburn-orange"   . "#DFAF8F")
    ("zenburn-yellow-2" . "#D0BF8F")
    ("zenburn-yellow-1" . "#E0CF9F")
    ("zenburn-yellow"   . "#F0DFAF")
    ("zenburn-green-5"  . "#2F4F2F")
    ("zenburn-green-4"  . "#3F5F3F")
    ("zenburn-green-3"  . "#4F6F4F")
    ("zenburn-green-2"  . "#5F7F5F")
    ("zenburn-green-1"  . "#6F8F6F")
    ("zenburn-green"    . "#7F9F7F")
    ("zenburn-green+1"  . "#8FB28F")
    ("zenburn-green+2"  . "#9FC59F")
    ("zenburn-green+3"  . "#AFD8AF")
    ("zenburn-green+4"  . "#BFEBBF")
    ("zenburn-cyan"     . "#93E0E3")
    ("zenburn-blue+3"   . "#BDE0F3")
    ("zenburn-blue+2"   . "#ACE0E3")
    ("zenburn-blue+1"   . "#94BFF3")
    ("zenburn-blue"     . "#8CD0D3")
    ("zenburn-blue-1"   . "#7CB8BB")
    ("zenburn-blue-2"   . "#6CA0A3")
    ("zenburn-blue-3"   . "#5C888B")
    ("zenburn-blue-4"   . "#4C7073")
    ("zenburn-blue-5"   . "#366060")
    ("zenburn-magenta"  . "#DC8CC3"))
  "List of Zenburn colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro zenburn-with-color-variables (&rest body)
  "`let' bind all colors defined in `zenburn-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
;;  `(let ((class `((class color) (min-colors 89)))
  `(let ((class `((type graphic)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append zenburn-default-colors-alist
                           zenburn-override-colors-alist))
         (z-variable-pitch (if zenburn-use-variable-pitch
                               'variable-pitch 'default)))
     ,@body))

;;; Theme Faces
(zenburn-with-color-variables
  (custom-theme-set-faces
   'zenburn

;;;; Built-in packages

;;;;; basic coloring
   `(button ((((type graphic)) (:underline t))))
   `(link ((((type graphic)) (:foreground ,zenburn-yellow :underline t :weight bold))))
   `(link-visited ((((type graphic)) (:foreground ,zenburn-yellow-2 :underline t :weight normal))))
   `(default ((((type graphic)) (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(cursor ((((type graphic)) (:foreground ,zenburn-fg :background ,zenburn-fg+1))))
   `(widget-field ((((type graphic)) (:foreground ,zenburn-fg :background ,zenburn-bg+3))))
   `(escape-glyph ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(fringe ((((type graphic)) (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
   `(header-line ((((type graphic)) (:foreground ,zenburn-yellow
                                  :background ,zenburn-bg-1
                                  :box (:line-width -1 :style released-button)
                                  :extend t))))
   `(highlight ((((type graphic)) (:background ,zenburn-bg-05))))
   `(success ((((type graphic)) (:foreground ,zenburn-green :weight bold))))
   `(warning ((((type graphic)) (:foreground ,zenburn-orange :weight bold))))
   `(tooltip ((((type graphic)) (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
;;;;; ansi-colors
   `(ansi-color-black ((((type graphic)) (:foreground ,zenburn-bg
                                       :background ,zenburn-bg-1))))
   `(ansi-color-red ((((type graphic)) (:foreground ,zenburn-red-2
                                     :background ,zenburn-red-4))))
   `(ansi-color-green ((((type graphic)) (:foreground ,zenburn-green
                                       :background ,zenburn-green+2))))
   `(ansi-color-yellow ((((type graphic)) (:foreground ,zenburn-orange
                                        :background ,zenburn-yellow))))
   `(ansi-color-blue ((((type graphic)) (:foreground ,zenburn-blue-1
                                      :background ,zenburn-blue-4))))
   `(ansi-color-magenta ((((type graphic)) (:foreground ,zenburn-magenta
                                         :background ,zenburn-red))))
   `(ansi-color-cyan ((((type graphic)) (:foreground ,zenburn-cyan
                                      :background ,zenburn-blue))))
   `(ansi-color-white ((((type graphic)) (:foreground ,zenburn-fg
                                       :background ,zenburn-fg-1))))
;;;;; compilation
   `(compilation-column-face ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(compilation-enter-directory-face ((((type graphic)) (:foreground ,zenburn-green))))
   `(compilation-error-face ((((type graphic)) (:foreground ,zenburn-red-1 :weight bold :underline t))))
   `(compilation-face ((((type graphic)) (:foreground ,zenburn-fg))))
   `(compilation-info-face ((((type graphic)) (:foreground ,zenburn-blue))))
   `(compilation-info ((((type graphic)) (:foreground ,zenburn-green+4 :underline t))))
   `(compilation-leave-directory-face ((((type graphic)) (:foreground ,zenburn-green))))
   `(compilation-line-face ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(compilation-line-number ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(compilation-message-face ((((type graphic)) (:foreground ,zenburn-blue))))
   `(compilation-warning-face ((((type graphic)) (:foreground ,zenburn-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((((type graphic)) (:foreground ,zenburn-green+2 :weight bold))))
   `(compilation-mode-line-fail ((((type graphic)) (:foreground ,zenburn-red :weight bold))))
   `(compilation-mode-line-run ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((((type graphic)) (:foreground ,zenburn-fg-1))))
   `(completions-common-part ((((type graphic)) (:foreground ,zenburn-blue))))
   `(completions-first-difference ((((type graphic)) (:foreground ,zenburn-fg+1))))
;;;;; customize
   `(custom-variable-tag ((((type graphic)) (:foreground ,zenburn-blue :weight bold))))
   `(custom-group-tag ((((type graphic)) (:foreground ,zenburn-blue :weight bold :height 1.2))))
   `(custom-state ((((type graphic)) (:foreground ,zenburn-green+4))))
;;;;; display-fill-column-indicator
   `(fill-column-indicator ((,class :foreground ,zenburn-bg-05 :weight semilight)))
;;;;; eww
   `(eww-invalid-certificate ((((type graphic)) (:inherit error))))
   `(eww-valid-certificate   ((((type graphic)) (:inherit success))))
;;;;; grep
   `(grep-context-face ((((type graphic)) (:foreground ,zenburn-fg))))
   `(grep-error-face ((((type graphic)) (:foreground ,zenburn-red-1 :weight bold :underline t))))
   `(grep-hit-face ((((type graphic)) (:foreground ,zenburn-blue))))
   `(grep-match-face ((((type graphic)) (:foreground ,zenburn-orange :weight bold))))
   `(match ((((type graphic)) (:background ,zenburn-bg-1 :foreground ,zenburn-orange :weight bold))))
;;;;; hi-lock
   `(hi-blue    ((((type graphic)) (:background ,zenburn-cyan    :foreground ,zenburn-bg-1))))
   `(hi-green   ((((type graphic)) (:background ,zenburn-green+4 :foreground ,zenburn-bg-1))))
   `(hi-pink    ((((type graphic)) (:background ,zenburn-magenta :foreground ,zenburn-bg-1))))
   `(hi-yellow  ((((type graphic)) (:background ,zenburn-yellow  :foreground ,zenburn-bg-1))))
   `(hi-blue-b  ((((type graphic)) (:foreground ,zenburn-blue    :weight     bold))))
   `(hi-green-b ((((type graphic)) (:foreground ,zenburn-green+2 :weight     bold))))
   `(hi-red-b   ((((type graphic)) (:foreground ,zenburn-red     :weight     bold))))
;;;;; info
   `(Info-quoted ((((type graphic)) (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((((type graphic)) (:foreground ,zenburn-yellow-2 :weight bold :background ,zenburn-bg+2))))
   `(isearch-fail ((((type graphic)) (:foreground ,zenburn-fg :background ,zenburn-red-4))))
   `(lazy-highlight ((((type graphic)) (:foreground ,zenburn-yellow-2 :weight bold :background ,zenburn-bg-05))))

   `(menu ((((type graphic)) (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(minibuffer-prompt ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(mode-line
     ((,class (:foreground ,zenburn-green+1
                           :background ,zenburn-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(mode-line-inactive
     ((((type graphic)) (:foreground ,zenburn-green-2
                      :background ,zenburn-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,zenburn-bg-1 :extend t))
             (t :inverse-video t)))
   `(secondary-selection ((((type graphic)) (:background ,zenburn-bg+2))))
   `(trailing-whitespace ((((type graphic)) (:background ,zenburn-red))))
   `(vertical-border ((((type graphic)) (:foreground ,zenburn-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((((type graphic)) (:foreground ,zenburn-fg :weight bold))))
   `(font-lock-comment-face ((((type graphic)) (:foreground ,zenburn-green))))
   `(font-lock-comment-delimiter-face ((((type graphic)) (:foreground ,zenburn-green-2))))
   `(font-lock-constant-face ((((type graphic)) (:foreground ,zenburn-green+4))))
   `(font-lock-doc-face ((((type graphic)) (:foreground ,zenburn-green+2))))
   `(font-lock-function-name-face ((((type graphic)) (:foreground ,zenburn-cyan))))
   `(font-lock-keyword-face ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(font-lock-negation-char-face ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(font-lock-preprocessor-face ((((type graphic)) (:foreground ,zenburn-blue+1))))
   `(font-lock-regexp-grouping-construct ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((((type graphic)) (:foreground ,zenburn-green :weight bold))))
   `(font-lock-string-face ((((type graphic)) (:foreground ,zenburn-red))))
   `(font-lock-type-face ((((type graphic)) (:foreground ,zenburn-blue-1))))
   `(font-lock-variable-name-face ((((type graphic)) (:foreground ,zenburn-orange))))
   `(font-lock-warning-face ((((type graphic)) (:foreground ,zenburn-yellow-2 :weight bold))))

   `(c-annotation-face ((((type graphic)) (:inherit font-lock-constant-face))))
;;;;; line numbers (Emacs 26.1 and above)
   `(line-number ((((type graphic)) (:inherit default :foreground ,zenburn-bg+3 :background ,zenburn-bg-05))))
   `(line-number-current-line ((((type graphic)) (:inherit line-number :foreground ,zenburn-yellow-2))))
;;;;; man
   `(Man-overstrike ((((type graphic)) (:inherit font-lock-keyword-face))))
   `(Man-underline  ((((type graphic)) (:inherit (font-lock-string-face underline)))))
;;;;; newsticker
   `(newsticker-date-face ((((type graphic)) (:foreground ,zenburn-fg))))
   `(newsticker-default-face ((((type graphic)) (:foreground ,zenburn-fg))))
   `(newsticker-enclosure-face ((((type graphic)) (:foreground ,zenburn-green+3))))
   `(newsticker-extra-face ((((type graphic)) (:foreground ,zenburn-bg+2 :height 0.8))))
   `(newsticker-feed-face ((((type graphic)) (:foreground ,zenburn-fg))))
   `(newsticker-immortal-item-face ((((type graphic)) (:foreground ,zenburn-green))))
   `(newsticker-new-item-face ((((type graphic)) (:foreground ,zenburn-blue))))
   `(newsticker-obsolete-item-face ((((type graphic)) (:foreground ,zenburn-red))))
   `(newsticker-old-item-face ((((type graphic)) (:foreground ,zenburn-bg+3))))
   `(newsticker-statistics-face ((((type graphic)) (:foreground ,zenburn-fg))))
   `(newsticker-treeview-face ((((type graphic)) (:foreground ,zenburn-fg))))
   `(newsticker-treeview-immortal-face ((((type graphic)) (:foreground ,zenburn-green))))
   `(newsticker-treeview-listwindow-face ((((type graphic)) (:foreground ,zenburn-fg))))
   `(newsticker-treeview-new-face ((((type graphic)) (:foreground ,zenburn-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((((type graphic)) (:foreground ,zenburn-red))))
   `(newsticker-treeview-old-face ((((type graphic)) (:foreground ,zenburn-bg+3))))
   `(newsticker-treeview-selection-face ((((type graphic)) (:background ,zenburn-bg-1 :foreground ,zenburn-yellow))))
;;;;; woman
   `(woman-bold   ((((type graphic)) (:inherit font-lock-keyword-face))))
   `(woman-italic ((((type graphic)) (:inherit (font-lock-string-face italic)))))

;;;; Third-party packages

;;;;; wgrep
   `(wgrep-grep-face ((((type graphic)) (:inherit isearch))))
;;;;; debbugs
   `(debbugs-gnu-done ((((type graphic)) (:foreground ,zenburn-fg-1))))
   `(debbugs-gnu-handled ((((type graphic)) (:foreground ,zenburn-green))))
   `(debbugs-gnu-new ((((type graphic)) (:foreground ,zenburn-red))))
   `(debbugs-gnu-pending ((((type graphic)) (:foreground ,zenburn-blue))))
   `(debbugs-gnu-stale ((((type graphic)) (:foreground ,zenburn-orange))))
   `(debbugs-gnu-tagged ((((type graphic)) (:foreground ,zenburn-red))))
;;;;; diff
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(diff-added          ((((type graphic)) (:background "#335533" :foreground ,zenburn-green))))
   `(diff-changed        ((((type graphic)) (:background "#555511" :foreground ,zenburn-yellow-1))))
   `(diff-removed        ((((type graphic)) (:background "#553333" :foreground ,zenburn-red-2))))
   `(diff-refine-added   ((((type graphic)) (:background "#338833" :foreground ,zenburn-green+4))))
   `(diff-refine-changed ((((type graphic)) (:background "#888811" :foreground ,zenburn-yellow))))
   `(diff-refine-removed ((((type graphic)) (:background "#883333" :foreground ,zenburn-red))))
   `(diff-header ((,class (:background ,zenburn-bg+2))
                  (t (:background ,zenburn-fg :foreground ,zenburn-bg))))
   `(diff-file-header
     ((,class (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))
      (t (:background ,zenburn-fg :foreground ,zenburn-bg :weight bold))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,zenburn-blue :background ,zenburn-blue-2))))
   `(diff-hl-delete ((,class (:foreground ,zenburn-red+1 :background ,zenburn-red-1))))
   `(diff-hl-insert ((,class (:foreground ,zenburn-green+1 :background ,zenburn-green-2))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((((type graphic)) :foreground ,zenburn-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((((type graphic)) (:foreground ,zenburn-blue))))
   `(diredp-compressed-file-suffix ((((type graphic)) (:foreground ,zenburn-orange))))
   `(diredp-date-time ((((type graphic)) (:foreground ,zenburn-magenta))))
   `(diredp-deletion ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(diredp-deletion-file-name ((((type graphic)) (:foreground ,zenburn-red))))
   `(diredp-dir-heading ((((type graphic)) (:foreground ,zenburn-blue :background ,zenburn-bg-1))))
   `(diredp-dir-priv ((((type graphic)) (:foreground ,zenburn-cyan))))
   `(diredp-exec-priv ((((type graphic)) (:foreground ,zenburn-red))))
   `(diredp-executable-tag ((((type graphic)) (:foreground ,zenburn-green+1))))
   `(diredp-file-name ((((type graphic)) (:foreground ,zenburn-blue))))
   `(diredp-file-suffix ((((type graphic)) (:foreground ,zenburn-green))))
   `(diredp-flag-mark ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(diredp-flag-mark-line ((((type graphic)) (:foreground ,zenburn-orange))))
   `(diredp-ignored-file-name ((((type graphic)) (:foreground ,zenburn-red))))
   `(diredp-link-priv ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(diredp-mode-line-flagged ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(diredp-mode-line-marked ((((type graphic)) (:foreground ,zenburn-orange))))
   `(diredp-no-priv ((((type graphic)) (:foreground ,zenburn-fg))))
   `(diredp-number ((((type graphic)) (:foreground ,zenburn-green+1))))
   `(diredp-other-priv ((((type graphic)) (:foreground ,zenburn-yellow-1))))
   `(diredp-rare-priv ((((type graphic)) (:foreground ,zenburn-red-1))))
   `(diredp-read-priv ((((type graphic)) (:foreground ,zenburn-green-2))))
   `(diredp-symlink ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(diredp-write-priv ((((type graphic)) (:foreground ,zenburn-magenta))))
;;;;; dired-async
   `(dired-async-failures ((((type graphic)) (:foreground ,zenburn-red :weight bold))))
   `(dired-async-message ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(dired-async-mode-message ((((type graphic)) (:foreground ,zenburn-yellow))))
;;;;; diredfl
   `(diredfl-compressed-file-suffix ((((type graphic)) (:foreground ,zenburn-orange))))
   `(diredfl-date-time ((((type graphic)) (:foreground ,zenburn-magenta))))
   `(diredfl-deletion ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(diredfl-deletion-file-name ((((type graphic)) (:foreground ,zenburn-red))))
   `(diredfl-dir-heading ((((type graphic)) (:foreground ,zenburn-blue :background ,zenburn-bg-1))))
   `(diredfl-dir-priv ((((type graphic)) (:foreground ,zenburn-cyan))))
   `(diredfl-exec-priv ((((type graphic)) (:foreground ,zenburn-red))))
   `(diredfl-executable-tag ((((type graphic)) (:foreground ,zenburn-green+1))))
   `(diredfl-file-name ((((type graphic)) (:foreground ,zenburn-blue))))
   `(diredfl-file-suffix ((((type graphic)) (:foreground ,zenburn-green))))
   `(diredfl-flag-mark ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(diredfl-flag-mark-line ((((type graphic)) (:foreground ,zenburn-orange))))
   `(diredfl-ignored-file-name ((((type graphic)) (:foreground ,zenburn-red))))
   `(diredfl-link-priv ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(diredfl-no-priv ((((type graphic)) (:foreground ,zenburn-fg))))
   `(diredfl-number ((((type graphic)) (:foreground ,zenburn-green+1))))
   `(diredfl-other-priv ((((type graphic)) (:foreground ,zenburn-yellow-1))))
   `(diredfl-rare-priv ((((type graphic)) (:foreground ,zenburn-red-1))))
   `(diredfl-read-priv ((((type graphic)) (:foreground ,zenburn-green-1))))
   `(diredfl-symlink ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(diredfl-write-priv ((((type graphic)) (:foreground ,zenburn-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((((type graphic)) (:foreground ,zenburn-fg :background ,zenburn-red-4))))
   `(ediff-current-diff-Ancestor ((((type graphic)) (:foreground ,zenburn-fg :background ,zenburn-red-4))))
   `(ediff-current-diff-B ((((type graphic)) (:foreground ,zenburn-fg :background ,zenburn-green-2))))
   `(ediff-current-diff-C ((((type graphic)) (:foreground ,zenburn-fg :background ,zenburn-blue-5))))
   `(ediff-even-diff-A ((((type graphic)) (:background ,zenburn-bg+1))))
   `(ediff-even-diff-Ancestor ((((type graphic)) (:background ,zenburn-bg+1))))
   `(ediff-even-diff-B ((((type graphic)) (:background ,zenburn-bg+1))))
   `(ediff-even-diff-C ((((type graphic)) (:background ,zenburn-bg+1))))
   `(ediff-fine-diff-A ((((type graphic)) (:foreground ,zenburn-fg :background ,zenburn-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((((type graphic)) (:foreground ,zenburn-fg :background ,zenburn-red-2 weight bold))))
   `(ediff-fine-diff-B ((((type graphic)) (:foreground ,zenburn-fg :background ,zenburn-green :weight bold))))
   `(ediff-fine-diff-C ((((type graphic)) (:foreground ,zenburn-fg :background ,zenburn-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((((type graphic)) (:background ,zenburn-bg+2))))
   `(ediff-odd-diff-Ancestor ((((type graphic)) (:background ,zenburn-bg+2))))
   `(ediff-odd-diff-B ((((type graphic)) (:background ,zenburn-bg+2))))
   `(ediff-odd-diff-C ((((type graphic)) (:background ,zenburn-bg+2))))
;;;;; emacs-w3m
   `(w3m-anchor ((((type graphic)) (:foreground ,zenburn-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((((type graphic)) (:foreground ,zenburn-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((((type graphic)) (:foreground ,zenburn-red-1 :underline t))))
   `(w3m-header-line-location-title ((((type graphic)) (:foreground ,zenburn-yellow
                                                     :underline t :weight bold))))
   `(w3m-history-current-url ((((type graphic)) (:inherit match))))
   `(w3m-lnum ((((type graphic)) (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
   `(w3m-lnum-match ((((type graphic)) (:background ,zenburn-bg-1
                                     :foreground ,zenburn-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((((type graphic)) (:foreground ,zenburn-yellow))))
;;;;; eshell
   `(eshell-prompt ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(eshell-ls-archive ((((type graphic)) (:foreground ,zenburn-red-1 :weight bold))))
   `(eshell-ls-backup ((((type graphic)) (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((((type graphic)) (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((((type graphic)) (:foreground ,zenburn-blue+1 :weight bold))))
   `(eshell-ls-executable ((((type graphic)) (:foreground ,zenburn-red+1 :weight bold))))
   `(eshell-ls-unreadable ((((type graphic)) (:foreground ,zenburn-fg))))
   `(eshell-ls-missing ((((type graphic)) (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((((type graphic)) (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(eshell-ls-symlink ((((type graphic)) (:foreground ,zenburn-cyan :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red-1) :inherit unspecified))
      (t (:foreground ,zenburn-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-yellow) :inherit unspecified))
      (t (:foreground ,zenburn-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-cyan) :inherit unspecified))
      (t (:foreground ,zenburn-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((((type graphic)) (:foreground ,zenburn-red-1 :weight bold))))
   `(flycheck-fringe-warning ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(flycheck-fringe-info ((((type graphic)) (:foreground ,zenburn-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburn-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburn-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburn-green-2 :weight bold :underline t))))
   `(flymake-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburn-red-1 :weight bold :underline t))))
   `(flymake-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburn-orange :weight bold :underline t))))
   `(flymake-note
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburn-green-2 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange) :inherit unspecified))
      (t (:foreground ,zenburn-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red) :inherit unspecified))
      (t (:foreground ,zenburn-red-1 :weight bold :underline t))))
;;;;; git-annex
   `(git-annex-dired-annexed-available ((((type graphic)) (:inherit success :weight normal))))
   `(git-annex-dired-annexed-unavailable ((((type graphic)) (:inherit error :weight normal))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,zenburn-green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,zenburn-blue+1  :weight bold)))) ; obsolete
   `(git-commit-comment-branch-local  ((,class (:foreground ,zenburn-blue+1  :weight bold))))
   `(git-commit-comment-branch-remote ((,class (:foreground ,zenburn-green  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,zenburn-yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((((type graphic)) (:foreground ,zenburn-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((((type graphic)) (:foreground ,zenburn-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((((type graphic)) (:foreground ,zenburn-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((((type graphic)) (:foreground ,zenburn-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((((type graphic)) (:foreground ,zenburn-green  :weight bold))))
   `(git-gutter-fr:deleted ((((type graphic)) (:foreground ,zenburn-red :weight bold))))
   `(git-gutter-fr:modified ((((type graphic)) (:foreground ,zenburn-magenta :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((((type graphic)) (:foreground, zenburn-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((((type graphic)) (:weight bold :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((((type graphic)) (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((((type graphic)) (:weight bold :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((((type graphic)) (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((((type graphic)) (:weight bold :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((((type graphic)) (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((((type graphic)) (:weight bold :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((((type graphic)) (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((((type graphic)) (:weight bold :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((((type graphic)) (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((((type graphic)) (:weight bold :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((((type graphic)) (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((((type graphic)) (:weight bold :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((((type graphic)) (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((((type graphic)) (:weight bold :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((((type graphic)) (:weight bold :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((((type graphic)) (:weight bold :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((((type graphic)) (:weight bold :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((((type graphic)) (:weight bold :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((((type graphic)) (:weight bold :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((((type graphic)) (:weight bold :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((((type graphic)) (:inherit message-header-other))))
   `(gnus-header-from ((((type graphic)) (:inherit message-header-to))))
   `(gnus-header-name ((((type graphic)) (:inherit message-header-name))))
   `(gnus-header-newsgroups ((((type graphic)) (:inherit message-header-other))))
   `(gnus-header-subject ((((type graphic)) (:inherit message-header-subject))))
   `(gnus-server-opened ((((type graphic)) (:foreground ,zenburn-green+2 :weight bold))))
   `(gnus-server-denied ((((type graphic)) (:foreground ,zenburn-red+1 :weight bold))))
   `(gnus-server-closed ((((type graphic)) (:foreground ,zenburn-blue :slant italic))))
   `(gnus-server-offline ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(gnus-server-agent ((((type graphic)) (:foreground ,zenburn-blue :weight bold))))
   `(gnus-summary-cancelled ((((type graphic)) (:foreground ,zenburn-orange))))
   `(gnus-summary-high-ancient ((((type graphic)) (:foreground ,zenburn-blue))))
   `(gnus-summary-high-read ((((type graphic)) (:foreground ,zenburn-green :weight bold))))
   `(gnus-summary-high-ticked ((((type graphic)) (:foreground ,zenburn-orange :weight bold))))
   `(gnus-summary-high-unread ((((type graphic)) (:foreground ,zenburn-fg :weight bold))))
   `(gnus-summary-low-ancient ((((type graphic)) (:foreground ,zenburn-blue))))
   `(gnus-summary-low-read ((((type graphic)) (:foreground ,zenburn-green))))
   `(gnus-summary-low-ticked ((((type graphic)) (:foreground ,zenburn-orange :weight bold))))
   `(gnus-summary-low-unread ((((type graphic)) (:foreground ,zenburn-fg))))
   `(gnus-summary-normal-ancient ((((type graphic)) (:foreground ,zenburn-blue))))
   `(gnus-summary-normal-read ((((type graphic)) (:foreground ,zenburn-green))))
   `(gnus-summary-normal-ticked ((((type graphic)) (:foreground ,zenburn-orange :weight bold))))
   `(gnus-summary-normal-unread ((((type graphic)) (:foreground ,zenburn-fg))))
   `(gnus-summary-selected ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(gnus-cite-1 ((((type graphic)) (:foreground ,zenburn-blue))))
   `(gnus-cite-10 ((((type graphic)) (:foreground ,zenburn-yellow-1))))
   `(gnus-cite-11 ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(gnus-cite-2 ((((type graphic)) (:foreground ,zenburn-blue-1))))
   `(gnus-cite-3 ((((type graphic)) (:foreground ,zenburn-blue-2))))
   `(gnus-cite-4 ((((type graphic)) (:foreground ,zenburn-green+2))))
   `(gnus-cite-5 ((((type graphic)) (:foreground ,zenburn-green+1))))
   `(gnus-cite-6 ((((type graphic)) (:foreground ,zenburn-green))))
   `(gnus-cite-7 ((((type graphic)) (:foreground ,zenburn-red))))
   `(gnus-cite-8 ((((type graphic)) (:foreground ,zenburn-red-1))))
   `(gnus-cite-9 ((((type graphic)) (:foreground ,zenburn-red-2))))
   `(gnus-group-news-1-empty ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(gnus-group-news-2-empty ((((type graphic)) (:foreground ,zenburn-green+3))))
   `(gnus-group-news-3-empty ((((type graphic)) (:foreground ,zenburn-green+1))))
   `(gnus-group-news-4-empty ((((type graphic)) (:foreground ,zenburn-blue-2))))
   `(gnus-group-news-5-empty ((((type graphic)) (:foreground ,zenburn-blue-3))))
   `(gnus-group-news-6-empty ((((type graphic)) (:foreground ,zenburn-bg+2))))
   `(gnus-group-news-low-empty ((((type graphic)) (:foreground ,zenburn-bg+2))))
   `(gnus-signature ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(gnus-x ((((type graphic)) (:background ,zenburn-fg :foreground ,zenburn-bg))))
   `(mm-uu-extract ((((type graphic)) (:background ,zenburn-bg-05 :foreground ,zenburn-green+1))))
;;;;; highlight-numbers
   `(highlight-numbers-number ((((type graphic)) (:foreground ,zenburn-blue))))
;;;;; highlight-symbol
   `(highlight-symbol-face ((((type graphic)) (:background ,zenburn-bg+2))))
;;;;; highlight-thing
   `(highlight-thing ((((type graphic)) (:background ,zenburn-bg+2))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,zenburn-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,zenburn-bg-05 :extend t)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,zenburn-bg+1))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((((type graphic)) (:foreground ,zenburn-red-1 :background ,zenburn-bg))))
   `(hydra-face-amaranth ((((type graphic)) (:foreground ,zenburn-red-3 :background ,zenburn-bg))))
   `(hydra-face-blue ((((type graphic)) (:foreground ,zenburn-blue :background ,zenburn-bg))))
   `(hydra-face-pink ((((type graphic)) (:foreground ,zenburn-magenta :background ,zenburn-bg))))
   `(hydra-face-teal ((((type graphic)) (:foreground ,zenburn-cyan :background ,zenburn-bg))))
;;;;; ido-mode
   `(ido-first-match ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(ido-only-match ((((type graphic)) (:foreground ,zenburn-orange :weight bold))))
   `(ido-subdir ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(ido-indicator ((((type graphic)) (:foreground ,zenburn-yellow :background ,zenburn-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((((type graphic)) (:background ,zenburn-bg+2 :weight bold))))
;;;; js2-mode
   `(js2-warning ((((type graphic)) (:underline ,zenburn-orange))))
   `(js2-error ((((type graphic)) (:foreground ,zenburn-red :weight bold))))
   `(js2-jsdoc-tag ((((type graphic)) (:foreground ,zenburn-green-2))))
   `(js2-jsdoc-type ((((type graphic)) (:foreground ,zenburn-green+2))))
   `(js2-jsdoc-value ((((type graphic)) (:foreground ,zenburn-green+3))))
   `(js2-function-param ((((type graphic)) (:foreground, zenburn-orange))))
   `(js2-external-variable ((((type graphic)) (:foreground ,zenburn-orange))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((((type graphic)) (:foreground ,zenburn-green-2))))
   `(js2-jsdoc-html-tag-delimiter ((((type graphic)) (:foreground ,zenburn-orange))))
   `(js2-jsdoc-html-tag-name ((((type graphic)) (:foreground ,zenburn-red-1))))
   `(js2-object-property ((((type graphic)) (:foreground ,zenburn-blue+1))))
   `(js2-magic-paren ((((type graphic)) (:foreground ,zenburn-blue-5))))
   `(js2-private-function-call ((((type graphic)) (:foreground ,zenburn-cyan))))
   `(js2-function-call ((((type graphic)) (:foreground ,zenburn-cyan))))
   `(js2-private-member ((((type graphic)) (:foreground ,zenburn-blue-1))))
   `(js2-keywords ((((type graphic)) (:foreground ,zenburn-magenta))))
;;;;; linum-mode
   `(linum ((((type graphic)) (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
;;;;; lispy
   `(lispy-command-name-face ((((type graphic)) (:background ,zenburn-bg-05 :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((((type graphic)) (:foreground ,zenburn-bg :background ,zenburn-fg))))
   `(lispy-face-hint ((((type graphic)) (:inherit highlight :foreground ,zenburn-yellow))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((((type graphic)) (:inherit 'ruler-mode-default :foreground ,zenburn-fg))))
   `(ruler-mode-fill-column ((((type graphic)) (:inherit 'ruler-mode-default :foreground ,zenburn-yellow))))
   `(ruler-mode-goal-column ((((type graphic)) (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((((type graphic)) (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((((type graphic)) (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((((type graphic)) (:foreground ,zenburn-yellow :box t))))
   `(ruler-mode-default ((((type graphic)) (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
;;;;; magit
;;;;;; headings and diffs
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(magit-section-highlight           ((((type graphic)) (:background ,zenburn-bg+05))))
   `(magit-section-heading             ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(magit-section-heading-selection   ((((type graphic)) (:foreground ,zenburn-orange :weight bold))))
   `(magit-diff-file-heading           ((((type graphic)) (:weight bold))))
   `(magit-diff-file-heading-highlight ((((type graphic)) (:background ,zenburn-bg+05 :weight bold))))
   `(magit-diff-file-heading-selection ((((type graphic)) (:background ,zenburn-bg+05 :weight bold
                                                        :foreground ,zenburn-orange))))
   `(magit-diff-added                  ((((type graphic)) (:background ,zenburn-green-2))))
   `(magit-diff-added-highlight        ((((type graphic)) (:background ,zenburn-green))))
   `(magit-diff-removed                ((((type graphic)) (:background ,zenburn-red-4))))
   `(magit-diff-removed-highlight      ((((type graphic)) (:background ,zenburn-red-3))))
   `(magit-diff-hunk-heading           ((((type graphic)) (:background ,zenburn-bg+1))))
   `(magit-diff-hunk-heading-highlight ((((type graphic)) (:background ,zenburn-bg+2))))
   `(magit-diff-hunk-heading-selection ((((type graphic)) (:background ,zenburn-bg+2
                                                        :foreground ,zenburn-orange))))
   `(magit-diff-lines-heading          ((((type graphic)) (:background ,zenburn-orange
                                                        :foreground ,zenburn-bg+2))))
   `(magit-diff-context-highlight      ((((type graphic)) (:background ,zenburn-bg+05
                                                        :foreground "grey70"))))
   `(magit-diffstat-added              ((((type graphic)) (:foreground ,zenburn-green+4))))
   `(magit-diffstat-removed            ((((type graphic)) (:foreground ,zenburn-red))))
;;;;;; popup
   `(magit-popup-heading             ((((type graphic)) (:foreground ,zenburn-yellow  :weight bold))))
   `(magit-popup-key                 ((((type graphic)) (:foreground ,zenburn-green-2 :weight bold))))
   `(magit-popup-argument            ((((type graphic)) (:foreground ,zenburn-green   :weight bold))))
   `(magit-popup-disabled-argument   ((((type graphic)) (:foreground ,zenburn-fg-1    :weight normal))))
   `(magit-popup-option-value        ((((type graphic)) (:foreground ,zenburn-blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((((type graphic)) (:foreground ,zenburn-green  :weight bold))))
   `(magit-process-ng    ((((type graphic)) (:foreground ,zenburn-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((((type graphic)) (:foreground ,zenburn-orange))))
   `(magit-log-date      ((((type graphic)) (:foreground ,zenburn-fg-1))))
   `(magit-log-graph     ((((type graphic)) (:foreground ,zenburn-fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((((type graphic)) (:foreground ,zenburn-yellow-2))))
   `(magit-sequence-stop ((((type graphic)) (:foreground ,zenburn-green))))
   `(magit-sequence-part ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(magit-sequence-head ((((type graphic)) (:foreground ,zenburn-blue))))
   `(magit-sequence-drop ((((type graphic)) (:foreground ,zenburn-red))))
   `(magit-sequence-done ((((type graphic)) (:foreground ,zenburn-fg-1))))
   `(magit-sequence-onto ((((type graphic)) (:foreground ,zenburn-fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((((type graphic)) (:foreground ,zenburn-green))))
   `(magit-bisect-skip ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(magit-bisect-bad  ((((type graphic)) (:foreground ,zenburn-red))))
;;;;;; blame
   `(magit-blame-heading ((((type graphic)) (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2))))
   `(magit-blame-hash    ((((type graphic)) (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2))))
   `(magit-blame-name    ((((type graphic)) (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))
   `(magit-blame-date    ((((type graphic)) (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))
   `(magit-blame-summary ((((type graphic)) (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((((type graphic)) (:foreground ,zenburn-bg+3))))
   `(magit-hash           ((((type graphic)) (:foreground ,zenburn-bg+3))))
   `(magit-tag            ((((type graphic)) (:foreground ,zenburn-orange :weight bold))))
   `(magit-branch-remote  ((((type graphic)) (:foreground ,zenburn-green  :weight bold))))
   `(magit-branch-local   ((((type graphic)) (:foreground ,zenburn-blue   :weight bold))))
   `(magit-branch-current ((((type graphic)) (:foreground ,zenburn-blue   :weight bold :box t))))
   `(magit-head           ((((type graphic)) (:foreground ,zenburn-blue   :weight bold))))
   `(magit-refname        ((((type graphic)) (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
   `(magit-refname-stash  ((((type graphic)) (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
   `(magit-refname-wip    ((((type graphic)) (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
   `(magit-signature-good      ((((type graphic)) (:foreground ,zenburn-green))))
   `(magit-signature-bad       ((((type graphic)) (:foreground ,zenburn-red))))
   `(magit-signature-untrusted ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(magit-signature-expired   ((((type graphic)) (:foreground ,zenburn-orange))))
   `(magit-signature-revoked   ((((type graphic)) (:foreground ,zenburn-magenta))))
   `(magit-signature-error     ((((type graphic)) (:inherit    magit-signature-bad))))
   `(magit-cherry-unmatched    ((((type graphic)) (:foreground ,zenburn-cyan))))
   `(magit-cherry-equivalent   ((((type graphic)) (:foreground ,zenburn-magenta))))
   `(magit-reflog-commit       ((((type graphic)) (:foreground ,zenburn-green))))
   `(magit-reflog-amend        ((((type graphic)) (:foreground ,zenburn-magenta))))
   `(magit-reflog-merge        ((((type graphic)) (:foreground ,zenburn-green))))
   `(magit-reflog-checkout     ((((type graphic)) (:foreground ,zenburn-blue))))
   `(magit-reflog-reset        ((((type graphic)) (:foreground ,zenburn-red))))
   `(magit-reflog-rebase       ((((type graphic)) (:foreground ,zenburn-magenta))))
   `(magit-reflog-cherry-pick  ((((type graphic)) (:foreground ,zenburn-green))))
   `(magit-reflog-remote       ((((type graphic)) (:foreground ,zenburn-cyan))))
   `(magit-reflog-other        ((((type graphic)) (:foreground ,zenburn-cyan))))
;;;;; markup-faces
   `(markup-anchor-face ((((type graphic)) (:foreground ,zenburn-blue+1))))
   `(markup-code-face ((((type graphic)) (:inherit font-lock-constant-face))))
   `(markup-command-face ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(markup-emphasis-face ((((type graphic)) (:inherit bold))))
   `(markup-internal-reference-face ((((type graphic)) (:foreground ,zenburn-yellow-2 :underline t))))
   `(markup-list-face ((((type graphic)) (:foreground ,zenburn-fg+1))))
   `(markup-meta-face ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(markup-meta-hide-face ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(markup-secondary-text-face ((((type graphic)) (:foreground ,zenburn-yellow-1))))
   `(markup-title-0-face ((((type graphic)) (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-1-face ((((type graphic)) (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-2-face ((((type graphic)) (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-3-face ((((type graphic)) (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-4-face ((((type graphic)) (:inherit font-lock-function-name-face :weight bold))))
   `(markup-typewriter-face ((((type graphic)) (:inherit font-lock-constant-face))))
   `(markup-verbatim-face ((((type graphic)) (:inherit font-lock-constant-face))))
   `(markup-value-face ((((type graphic)) (:foreground ,zenburn-yellow))))
;;;;; message-mode
   `(message-cited-text ((((type graphic)) (:inherit font-lock-comment-face))))
   `(message-header-name ((((type graphic)) (:foreground ,zenburn-green+1))))
   `(message-header-other ((((type graphic)) (:foreground ,zenburn-green))))
   `(message-header-to ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(message-header-cc ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(message-header-newsgroups ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(message-header-subject ((((type graphic)) (:foreground ,zenburn-orange :weight bold))))
   `(message-header-xheader ((((type graphic)) (:foreground ,zenburn-green))))
   `(message-mml ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(message-separator ((((type graphic)) (:inherit font-lock-comment-face))))
;;;;; mu4e
   `(mu4e-cited-1-face ((((type graphic)) (:foreground ,zenburn-blue    :slant italic))))
   `(mu4e-cited-2-face ((((type graphic)) (:foreground ,zenburn-green+2 :slant italic))))
   `(mu4e-cited-3-face ((((type graphic)) (:foreground ,zenburn-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((((type graphic)) (:foreground ,zenburn-green   :slant italic))))
   `(mu4e-cited-5-face ((((type graphic)) (:foreground ,zenburn-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((((type graphic)) (:foreground ,zenburn-green-2 :slant italic))))
   `(mu4e-cited-7-face ((((type graphic)) (:foreground ,zenburn-blue    :slant italic))))
   `(mu4e-replied-face ((((type graphic)) (:foreground ,zenburn-bg+3))))
   `(mu4e-trashed-face ((((type graphic)) (:foreground ,zenburn-bg+3 :strike-through t))))
;;;;; orderless
   `(orderless-match-face-0 ((((type graphic)) (:foreground ,zenburn-green))))
   `(orderless-match-face-1 ((((type graphic)) (:foreground ,zenburn-magenta))))
   `(orderless-match-face-2 ((((type graphic)) (:foreground ,zenburn-blue))))
   `(orderless-match-face-3 ((((type graphic)) (:foreground ,zenburn-orange))))
;;;;; org-mode
   `(org-agenda-date-today
     ((((type graphic)) (:foreground ,zenburn-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((((type graphic)) (:inherit font-lock-comment-face))))
   `(org-archived ((((type graphic)) (:foreground ,zenburn-fg :weight bold))))
   `(org-block ((((type graphic)) (:background ,zenburn-bg+05 :extend t))))
   `(org-checkbox ((((type graphic)) (:background ,zenburn-bg+2 :foreground ,zenburn-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((((type graphic)) (:foreground ,zenburn-blue :underline t))))
   `(org-deadline-announce ((((type graphic)) (:foreground ,zenburn-red-1))))
   `(org-done ((((type graphic)) (:weight bold :weight bold :foreground ,zenburn-green+3))))
   `(org-formula ((((type graphic)) (:foreground ,zenburn-yellow-2))))
   `(org-headline-done ((((type graphic)) (:foreground ,zenburn-green+3))))
   `(org-hide ((((type graphic)) (:foreground ,zenburn-bg))))
   `(org-level-1 ((((type graphic)) (:inherit ,z-variable-pitch :foreground ,zenburn-orange
                               ,@(when zenburn-scale-org-headlines
                                   (list :height zenburn-height-plus-4))))))
   `(org-level-2 ((((type graphic)) (:inherit ,z-variable-pitch :foreground ,zenburn-green+4
                               ,@(when zenburn-scale-org-headlines
                                   (list :height zenburn-height-plus-3))))))
   `(org-level-3 ((((type graphic)) (:inherit ,z-variable-pitch :foreground ,zenburn-blue-1
                               ,@(when zenburn-scale-org-headlines
                                   (list :height zenburn-height-plus-2))))))
   `(org-level-4 ((((type graphic)) (:inherit ,z-variable-pitch :foreground ,zenburn-yellow-2
                               ,@(when zenburn-scale-org-headlines
                                   (list :height zenburn-height-plus-1))))))
   `(org-level-5 ((((type graphic)) (:inherit ,z-variable-pitch :foreground ,zenburn-cyan))))
   `(org-level-6 ((((type graphic)) (:inherit ,z-variable-pitch :foreground ,zenburn-green+2))))
   `(org-level-7 ((((type graphic)) (:inherit ,z-variable-pitch :foreground ,zenburn-red+2))))
   `(org-level-8 ((((type graphic)) (:inherit ,z-variable-pitch :foreground ,zenburn-magenta))))
   `(org-link ((((type graphic)) (:foreground ,zenburn-yellow-2 :underline t))))
   `(org-quote ((((type graphic)) (:background ,zenburn-bg+05 :extend t))))
   `(org-scheduled ((((type graphic)) (:foreground ,zenburn-green+4))))
   `(org-scheduled-previously ((((type graphic)) (:foreground ,zenburn-red))))
   `(org-scheduled-today ((((type graphic)) (:foreground ,zenburn-blue+1))))
   `(org-sexp-date ((((type graphic)) (:foreground ,zenburn-blue+1 :underline t))))
   `(org-special-keyword ((((type graphic)) (:inherit font-lock-comment-face))))
   `(org-table ((((type graphic)) (:foreground ,zenburn-green+2))))
   `(org-tag ((((type graphic)) (:weight bold :weight bold))))
   `(org-time-grid ((((type graphic)) (:foreground ,zenburn-orange))))
   `(org-todo ((((type graphic)) (:weight bold :foreground ,zenburn-red :weight bold))))
   `(org-upcoming-deadline ((((type graphic)) (:inherit font-lock-keyword-face))))
   `(org-warning ((((type graphic)) (:weight bold :foreground ,zenburn-red :weight bold :underline nil))))
   `(org-column ((((type graphic)) (:background ,zenburn-bg-1))))
   `(org-column-title ((((type graphic)) (:background ,zenburn-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((((type graphic)) (:foreground ,zenburn-fg :background ,zenburn-bg-1))))
   `(org-mode-line-clock-overrun ((((type graphic)) (:foreground ,zenburn-bg :background ,zenburn-red-1))))
   `(org-ellipsis ((((type graphic)) (:foreground ,zenburn-yellow-1 :underline t))))
   `(org-footnote ((((type graphic)) (:foreground ,zenburn-cyan :underline t))))
   `(org-document-title ((((type graphic)) (:inherit ,z-variable-pitch :foreground ,zenburn-blue
                                      :weight bold
                                      ,@(when zenburn-scale-org-headlines
                                          (list :height zenburn-height-plus-4))))))
   `(org-document-info ((((type graphic)) (:foreground ,zenburn-blue))))
   `(org-habit-ready-face ((((type graphic)) :background ,zenburn-green)))
   `(org-habit-alert-face ((((type graphic)) :background ,zenburn-yellow-1 :foreground ,zenburn-bg)))
   `(org-habit-clear-face ((((type graphic)) :background ,zenburn-blue-3)))
   `(org-habit-overdue-face ((((type graphic)) :background ,zenburn-red-3)))
   `(org-habit-clear-future-face ((((type graphic)) :background ,zenburn-blue-4)))
   `(org-habit-ready-future-face ((((type graphic)) :background ,zenburn-green-2)))
   `(org-habit-alert-future-face ((((type graphic)) :background ,zenburn-yellow-2 :foreground ,zenburn-bg)))
   `(org-habit-overdue-future-face ((((type graphic)) :background ,zenburn-red-4)))
;;;;; org-ref
   `(org-ref-ref-face ((((type graphic)) :underline t)))
   `(org-ref-label-face ((((type graphic)) :underline t)))
   `(org-ref-cite-face ((((type graphic)) :underline t)))
   `(org-ref-glossary-face ((((type graphic)) :underline t)))
   `(org-ref-acronym-face ((((type graphic)) :underline t)))
;;;;; outline
   `(outline-1 ((((type graphic)) (:inherit ,z-variable-pitch :foreground ,zenburn-orange
                             ,@(when zenburn-scale-outline-headlines
                                 (list :height zenburn-height-plus-4))))))
   `(outline-2 ((((type graphic)) (:inherit ,z-variable-pitch :foreground ,zenburn-green+4
                             ,@(when zenburn-scale-outline-headlines
                                 (list :height zenburn-height-plus-3))))))
   `(outline-3 ((((type graphic)) (:inherit ,z-variable-pitch :foreground ,zenburn-blue-1
                             ,@(when zenburn-scale-outline-headlines
                                 (list :height zenburn-height-plus-2))))))
   `(outline-4 ((((type graphic)) (:inherit ,z-variable-pitch :foreground ,zenburn-yellow-2
                             ,@(when zenburn-scale-outline-headlines
                                 (list :height zenburn-height-plus-1))))))
   `(outline-5 ((((type graphic)) (:inherit ,z-variable-pitch :foreground ,zenburn-cyan))))
   `(outline-6 ((((type graphic)) (:inherit ,z-variable-pitch :foreground ,zenburn-green+2))))
   `(outline-7 ((((type graphic)) (:inherit ,z-variable-pitch :foreground ,zenburn-red-4))))
   `(outline-8 ((((type graphic)) (:inherit ,z-variable-pitch :foreground ,zenburn-blue-4))))
;;;;; c/perl
   `(cperl-nonoverridable-face ((((type graphic)) (:foreground ,zenburn-magenta))))
   `(cperl-array-face ((((type graphic)) (:foreground ,zenburn-yellow, :background ,zenburn-bg))))
   `(cperl-hash-face ((((type graphic)) (:foreground ,zenburn-yellow-1, :background ,zenburn-bg))))
;;;;; paren-face
   `(parenthesis ((((type graphic)) (:foreground ,zenburn-fg-1))))
;;;;; perspective
   `(persp-selected-face ((((type graphic)) (:foreground ,zenburn-yellow-2))))
;;;;; powerline
   `(powerline-active1 ((((type graphic)) (:background ,zenburn-bg-05 :inherit mode-line))))
   `(powerline-active2 ((((type graphic)) (:background ,zenburn-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((((type graphic)) (:background ,zenburn-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((((type graphic)) (:background ,zenburn-bg+3 :inherit mode-line-inactive))))
;;;;; re-builder
   `(reb-match-0 ((((type graphic)) (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(reb-match-1 ((((type graphic)) (:foreground ,zenburn-bg :background ,zenburn-blue))))
   `(reb-match-2 ((((type graphic)) (:foreground ,zenburn-bg :background ,zenburn-orange))))
   `(reb-match-3 ((((type graphic)) (:foreground ,zenburn-bg :background ,zenburn-red))))
;;;;; rmail
   `(rmail-highlight ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(rmail-header-name ((((type graphic)) (:foreground ,zenburn-blue))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((((type graphic)) (:foreground ,zenburn-green))))
   `(rpm-spec-doc-face ((((type graphic)) (:foreground ,zenburn-green))))
   `(rpm-spec-ghost-face ((((type graphic)) (:foreground ,zenburn-red))))
   `(rpm-spec-macro-face ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(rpm-spec-obsolete-tag-face ((((type graphic)) (:foreground ,zenburn-red))))
   `(rpm-spec-package-face ((((type graphic)) (:foreground ,zenburn-red))))
   `(rpm-spec-section-face ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(rpm-spec-tag-face ((((type graphic)) (:foreground ,zenburn-blue))))
   `(rpm-spec-var-face ((((type graphic)) (:foreground ,zenburn-red))))
;;;;; rst-mode
   `(rst-level-1-face ((((type graphic)) (:foreground ,zenburn-orange))))
   `(rst-level-2-face ((((type graphic)) (:foreground ,zenburn-green+1))))
   `(rst-level-3-face ((((type graphic)) (:foreground ,zenburn-blue-1))))
   `(rst-level-4-face ((((type graphic)) (:foreground ,zenburn-yellow-2))))
   `(rst-level-5-face ((((type graphic)) (:foreground ,zenburn-cyan))))
   `(rst-level-6-face ((((type graphic)) (:foreground ,zenburn-green-2))))
;;;;; selectrum
   `(selectrum-current-candidate ((((type graphic)) (:foreground ,zenburn-yellow :weight bold :underline t))))
   `(selectrum-primary-highlight ((((type graphic)) (:background ,zenburn-green-2))))
   `(selectrum-secondary-highlight ((((type graphic)) (:background ,zenburn-green))))
;;;;; sh-mode
   `(sh-heredoc     ((((type graphic)) (:foreground ,zenburn-yellow :weight bold))))
   `(sh-quoted-exec ((((type graphic)) (:foreground ,zenburn-red))))
;;;;; show-paren
   `(show-paren-mismatch ((((type graphic)) (:foreground ,zenburn-red+1 :background ,zenburn-bg+3 :weight bold))))
   `(show-paren-match ((((type graphic)) (:foreground ,zenburn-fg :background ,zenburn-bg+3 :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable Zenburn for sml
   `(sml/global ((,class (:foreground ,zenburn-fg :weight bold))))
   `(sml/modes ((,class (:foreground ,zenburn-yellow :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,zenburn-fg-1 :weight bold))))
   `(sml/filename ((,class (:foreground ,zenburn-yellow :weight bold))))
   `(sml/line-number ((,class (:foreground ,zenburn-blue :weight bold))))
   `(sml/col-number ((,class (:foreground ,zenburn-blue+1 :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,zenburn-blue-1 :weight bold))))
   `(sml/prefix ((,class (:foreground ,zenburn-orange))))
   `(sml/git ((,class (:foreground ,zenburn-green+3))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,zenburn-orange :weight bold))))
   `(sml/read-only ((,class (:foreground ,zenburn-red-2))))
   `(sml/outside-modified ((,class (:foreground ,zenburn-orange))))
   `(sml/modified ((,class (:foreground ,zenburn-red))))
   `(sml/vc-edited ((,class (:foreground ,zenburn-green+2))))
   `(sml/charging ((,class (:foreground ,zenburn-green+4))))
   `(sml/discharging ((,class (:foreground ,zenburn-red+1))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((((type graphic)) (:foreground ,zenburn-red+1 :background ,zenburn-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((((type graphic)) (:background ,zenburn-bg+3 :weight bold))))
;;;;; sml-mode-line
   `(sml-modeline-end-face ((((type graphic)) :inherit default :width condensed)))
;;;;; term
   `(term-color-black ((((type graphic)) (:foreground ,zenburn-bg
                                       :background ,zenburn-bg-1))))
   `(term-color-red ((((type graphic)) (:foreground ,zenburn-red-2
                                     :background ,zenburn-red-4))))
   `(term-color-green ((((type graphic)) (:foreground ,zenburn-green
                                       :background ,zenburn-green+2))))
   `(term-color-yellow ((((type graphic)) (:foreground ,zenburn-orange
                                        :background ,zenburn-yellow))))
   `(term-color-blue ((((type graphic)) (:foreground ,zenburn-blue-1
                                      :background ,zenburn-blue-4))))
   `(term-color-magenta ((((type graphic)) (:foreground ,zenburn-magenta
                                         :background ,zenburn-red))))
   `(term-color-cyan ((((type graphic)) (:foreground ,zenburn-cyan
                                      :background ,zenburn-blue))))
   `(term-color-white ((((type graphic)) (:foreground ,zenburn-fg
                                       :background ,zenburn-fg-1))))
   `(term-default-fg-color ((((type graphic)) (:inherit term-color-white))))
   `(term-default-bg-color ((((type graphic)) (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((((type graphic)) (:foreground ,zenburn-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((((type graphic)) (:foreground ,zenburn-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((((type graphic)) (:foreground ,zenburn-fg))))
   `(undo-tree-visualizer-register-face ((((type graphic)) (:foreground ,zenburn-yellow))))
   `(undo-tree-visualizer-unmodified-face ((((type graphic)) (:foreground ,zenburn-cyan))))
;;;;; vertico
   `(vertico-current ((((type graphic)) (:foreground ,zenburn-yellow :weight bold :underline t))))
;;;;; visual-regexp
   `(vr/group-0 ((((type graphic)) (:foreground ,zenburn-bg :background ,zenburn-green :weight bold))))
   `(vr/group-1 ((((type graphic)) (:foreground ,zenburn-bg :background ,zenburn-orange :weight bold))))
   `(vr/group-2 ((((type graphic)) (:foreground ,zenburn-bg :background ,zenburn-blue :weight bold))))
   `(vr/match-0 ((((type graphic)) (:inherit isearch))))
   `(vr/match-1 ((((type graphic)) (:foreground ,zenburn-yellow-2 :background ,zenburn-bg-1 :weight bold))))
   `(vr/match-separator-face ((((type graphic)) (:foreground ,zenburn-red :weight bold))))
;;;;; volatile-highlights
   `(vhl/default-face ((((type graphic)) (:background ,zenburn-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((((type graphic)) (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((((type graphic)) (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((((type graphic)) (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((((type graphic)) (:foreground ,zenburn-orange ))))
   `(web-mode-css-prop-face ((((type graphic)) (:foreground ,zenburn-orange))))
   `(web-mode-css-pseudo-class-face ((((type graphic)) (:foreground ,zenburn-green+3 :weight bold))))
   `(web-mode-css-rule-face ((((type graphic)) (:foreground ,zenburn-blue))))
   `(web-mode-doctype-face ((((type graphic)) (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((((type graphic)) (:underline t))))
   `(web-mode-function-name-face ((((type graphic)) (:foreground ,zenburn-blue))))
   `(web-mode-html-attr-name-face ((((type graphic)) (:foreground ,zenburn-orange))))
   `(web-mode-html-attr-value-face ((((type graphic)) (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((((type graphic)) (:foreground ,zenburn-cyan))))
   `(web-mode-keyword-face ((((type graphic)) (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((((type graphic)) (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((((type graphic)) (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((((type graphic)) (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((((type graphic)) (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((((type graphic)) (:background ,zenburn-bg))))
   `(web-mode-server-comment-face ((((type graphic)) (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((((type graphic)) (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((((type graphic)) (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((((type graphic)) (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((((type graphic)) (:background ,zenburn-red))))
;;;;; whitespace-mode
   `(whitespace-space ((((type graphic)) (:background ,zenburn-bg+1 :foreground ,zenburn-bg+1))))
   `(whitespace-hspace ((((type graphic)) (:background ,zenburn-bg+1 :foreground ,zenburn-bg+1))))
   `(whitespace-tab ((((type graphic)) (:background ,zenburn-red-1))))
   `(whitespace-newline ((((type graphic)) (:foreground ,zenburn-bg+1))))
   `(whitespace-trailing ((((type graphic)) (:background ,zenburn-red))))
   `(whitespace-line ((((type graphic)) (:background ,zenburn-bg :foreground ,zenburn-magenta))))
   `(whitespace-space-before-tab ((((type graphic)) (:background ,zenburn-orange :foreground ,zenburn-orange))))
   `(whitespace-indentation ((((type graphic)) (:background ,zenburn-yellow :foreground ,zenburn-red))))
   `(whitespace-empty ((((type graphic)) (:background ,zenburn-yellow))))
   `(whitespace-space-after-tab ((((type graphic)) (:background ,zenburn-yellow :foreground ,zenburn-red))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((((type graphic)) (:background ,zenburn-bg-1))))
   `(yascroll:thumb-fringe ((((type graphic)) (:background ,zenburn-bg-1 :foreground ,zenburn-bg-1))))
   ))

;;; Theme Variables
(zenburn-with-color-variables
  (custom-theme-set-variables
   'zenburn
;;;;; ansi-color
   `(ansi-color-names-vector [,zenburn-bg ,zenburn-red ,zenburn-green ,zenburn-yellow
                                          ,zenburn-blue ,zenburn-magenta ,zenburn-cyan ,zenburn-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,zenburn-bg-05)
;;;;; pdf-tools
   `(pdf-view-midnight-colors `(,zenburn-fg . ,zenburn-bg-05))
;;;;; vc-annotate
   `(vc-annotate-color-map
     `(( 20. . ,zenburn-red-1)
       ( 40. . ,zenburn-red)
       ( 60. . ,zenburn-orange)
       ( 80. . ,zenburn-yellow-2)
       (100. . ,zenburn-yellow-1)
       (120. . ,zenburn-yellow)
       (140. . ,zenburn-green-2)
       (160. . ,zenburn-green)
       (180. . ,zenburn-green+1)
       (200. . ,zenburn-green+2)
       (220. . ,zenburn-green+3)
       (240. . ,zenburn-green+4)
       (260. . ,zenburn-cyan)
       (280. . ,zenburn-blue-2)
       (300. . ,zenburn-blue-1)
       (320. . ,zenburn-blue)
       (340. . ,zenburn-blue+1)
       (360. . ,zenburn-magenta)))
   `(vc-annotate-very-old-color ,zenburn-magenta)
   `(vc-annotate-background ,zenburn-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defcustom zenburn-add-font-lock-keywords nil
  "Whether to add font-lock keywords for zenburn color names.

In buffers visiting library `zenburn-theme.el' the zenburn
specific keywords are always added, provided that library has
been loaded (because that is where the code that does it is
defined).  If you visit this file and only enable the theme,
then you have to turn `rainbow-mode' off and on again for the
zenburn-specific font-lock keywords to be used.

In all other Emacs-Lisp buffers this variable controls whether
this should be done.  This requires library `rainbow-mode'."
  :type 'boolean
  :group 'zenburn-theme)

(defvar zenburn-colors-font-lock-keywords nil)

(defun zenburn--rainbow-turn-on ()
  "Maybe also add font-lock keywords for zenburn colors."
  (when (and (derived-mode-p 'emacs-lisp-mode)
             (or zenburn-add-font-lock-keywords
                 (and (buffer-file-name)
                      (equal (file-name-nondirectory (buffer-file-name))
                             "zenburn-theme.el"))))
    (unless zenburn-colors-font-lock-keywords
      (setq zenburn-colors-font-lock-keywords
            `((,(regexp-opt (mapcar 'car zenburn-default-colors-alist) 'words)
               (0 (rainbow-colorize-by-assoc zenburn-default-colors-alist))))))
    (font-lock-add-keywords nil zenburn-colors-font-lock-keywords 'end)))

(defun zenburn--rainbow-turn-off ()
  "Also remove font-lock keywords for zenburn colors."
  (font-lock-remove-keywords nil zenburn-colors-font-lock-keywords))

(when (fboundp 'advice-add)
  (advice-add 'rainbow-turn-on :after  #'zenburn--rainbow-turn-on)
  (advice-add 'rainbow-turn-off :after #'zenburn--rainbow-turn-off))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'zenburn)

;;; zenburn-theme.el ends here
