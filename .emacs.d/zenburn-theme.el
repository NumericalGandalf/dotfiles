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
    `(button ((,class (:underline t))))
    `(link ((,class (:foreground ,zenburn-yellow :underline t :weight bold))))
    `(link-visited ((,class (:foreground ,zenburn-yellow-2 :underline t :weight normal))))
    `(default ((,class (:foreground ,zenburn-fg :background ,zenburn-bg))))
    `(cursor ((,class (:foreground ,zenburn-fg :background ,zenburn-fg+1))))
    `(widget-field ((,class (:foreground ,zenburn-fg :background ,zenburn-bg+3))))
    `(escape-glyph ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(fringe ((,class (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
    `(header-line ((,class (:foreground ,zenburn-yellow
                                       :background ,zenburn-bg-1
                                       :box (:line-width -1 :style released-button)
                                       :extend t))))
    `(highlight ((,class (:background ,zenburn-bg-05))))
    `(success ((,class (:foreground ,zenburn-green :weight bold))))
    `(warning ((,class (:foreground ,zenburn-orange :weight bold))))
    `(tooltip ((,class (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
;;;;; ansi-colors
    `(ansi-color-black ((,class (:foreground ,zenburn-bg
                                            :background ,zenburn-bg-1))))
    `(ansi-color-red ((,class (:foreground ,zenburn-red-2
                                          :background ,zenburn-red-4))))
    `(ansi-color-green ((,class (:foreground ,zenburn-green
                                            :background ,zenburn-green+2))))
    `(ansi-color-yellow ((,class (:foreground ,zenburn-orange
                                             :background ,zenburn-yellow))))
    `(ansi-color-blue ((,class (:foreground ,zenburn-blue-1
                                           :background ,zenburn-blue-4))))
    `(ansi-color-magenta ((,class (:foreground ,zenburn-magenta
                                              :background ,zenburn-red))))
    `(ansi-color-cyan ((,class (:foreground ,zenburn-cyan
                                           :background ,zenburn-blue))))
    `(ansi-color-white ((,class (:foreground ,zenburn-fg
                                            :background ,zenburn-fg-1))))
;;;;; compilation
    `(compilation-column-face ((,class (:foreground ,zenburn-yellow))))
    `(compilation-enter-directory-face ((,class (:foreground ,zenburn-green))))
    `(compilation-error-face ((,class (:foreground ,zenburn-red-1 :weight bold :underline t))))
    `(compilation-face ((,class (:foreground ,zenburn-fg))))
    `(compilation-info-face ((,class (:foreground ,zenburn-blue))))
    `(compilation-info ((,class (:foreground ,zenburn-green+4 :underline t))))
    `(compilation-leave-directory-face ((,class (:foreground ,zenburn-green))))
    `(compilation-line-face ((,class (:foreground ,zenburn-yellow))))
    `(compilation-line-number ((,class (:foreground ,zenburn-yellow))))
    `(compilation-message-face ((,class (:foreground ,zenburn-blue))))
    `(compilation-warning-face ((,class (:foreground ,zenburn-orange :weight bold :underline t))))
    `(compilation-mode-line-exit ((,class (:foreground ,zenburn-green+2 :weight bold))))
    `(compilation-mode-line-fail ((,class (:foreground ,zenburn-red :weight bold))))
    `(compilation-mode-line-run ((,class (:foreground ,zenburn-yellow :weight bold))))
;;;;; completions
    `(completions-annotations ((,class (:foreground ,zenburn-fg-1))))
    `(completions-common-part ((,class (:foreground ,zenburn-blue))))
    `(completions-first-difference ((,class (:foreground ,zenburn-fg+1))))
;;;;; customize
    `(custom-variable-tag ((,class (:foreground ,zenburn-blue :weight bold))))
    `(custom-group-tag ((,class (:foreground ,zenburn-blue :weight bold :height 1.2))))
    `(custom-state ((,class (:foreground ,zenburn-green+4))))
;;;;; display-fill-column-indicator
    `(fill-column-indicator ((,class :foreground ,zenburn-bg-05 :weight semilight)))
;;;;; eww
    `(eww-invalid-certificate ((,class (:inherit error))))
    `(eww-valid-certificate   ((,class (:inherit success))))
;;;;; grep
    `(grep-context-face ((,class (:foreground ,zenburn-fg))))
    `(grep-error-face ((,class (:foreground ,zenburn-red-1 :weight bold :underline t))))
    `(grep-hit-face ((,class (:foreground ,zenburn-blue))))
    `(grep-match-face ((,class (:foreground ,zenburn-orange :weight bold))))
    `(match ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-orange :weight bold))))
;;;;; hi-lock
    `(hi-blue    ((,class (:background ,zenburn-cyan    :foreground ,zenburn-bg-1))))
    `(hi-green   ((,class (:background ,zenburn-green+4 :foreground ,zenburn-bg-1))))
    `(hi-pink    ((,class (:background ,zenburn-magenta :foreground ,zenburn-bg-1))))
    `(hi-yellow  ((,class (:background ,zenburn-yellow  :foreground ,zenburn-bg-1))))
    `(hi-blue-b  ((,class (:foreground ,zenburn-blue    :weight     bold))))
    `(hi-green-b ((,class (:foreground ,zenburn-green+2 :weight     bold))))
    `(hi-red-b   ((,class (:foreground ,zenburn-red     :weight     bold))))
;;;;; info
    `(Info-quoted ((,class (:inherit font-lock-constant-face))))
;;;;; isearch
    `(isearch ((,class (:foreground ,zenburn-yellow-2 :weight bold :background ,zenburn-bg+2))))
    `(isearch-fail ((,class (:foreground ,zenburn-fg :background ,zenburn-red-4))))
    `(lazy-highlight ((,class (:foreground ,zenburn-yellow-2 :weight bold :background ,zenburn-bg-05))))

    `(menu ((,class (:foreground ,zenburn-fg :background ,zenburn-bg))))
    `(minibuffer-prompt ((,class (:foreground ,zenburn-yellow))))
    `(mode-line
       ((,class (:foreground ,zenburn-green+1
                  :background ,zenburn-bg-1
                  :box (:line-width -1 :style released-button)))
         (t :inverse-video t)))
    `(mode-line-buffer-id ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(mode-line-inactive
       ((,class (:foreground ,zenburn-green-2
                            :background ,zenburn-bg-05
                            :box (:line-width -1 :style released-button)))))
    `(region ((,class (:background ,zenburn-bg-1 :extend t))
               (t :inverse-video t)))
    `(secondary-selection ((,class (:background ,zenburn-bg+2))))
    `(trailing-whitespace ((,class (:background ,zenburn-red))))
    `(vertical-border ((,class (:foreground ,zenburn-fg))))
;;;;; font lock
    `(font-lock-builtin-face ((,class (:foreground ,zenburn-fg :weight bold))))
    `(font-lock-comment-face ((,class (:foreground ,zenburn-green))))
    `(font-lock-comment-delimiter-face ((,class (:foreground ,zenburn-green-2))))
    `(font-lock-constant-face ((,class (:foreground ,zenburn-green+4))))
    `(font-lock-doc-face ((,class (:foreground ,zenburn-green+2))))
    `(font-lock-function-name-face ((,class (:foreground ,zenburn-cyan))))
    `(font-lock-keyword-face ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(font-lock-negation-char-face ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(font-lock-preprocessor-face ((,class (:foreground ,zenburn-blue+1))))
    `(font-lock-regexp-grouping-construct ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(font-lock-regexp-grouping-backslash ((,class (:foreground ,zenburn-green :weight bold))))
    `(font-lock-string-face ((,class (:foreground ,zenburn-red))))
    `(font-lock-type-face ((,class (:foreground ,zenburn-blue-1))))
    `(font-lock-variable-name-face ((,class (:foreground ,zenburn-orange))))
    `(font-lock-warning-face ((,class (:foreground ,zenburn-yellow-2 :weight bold))))

    `(c-annotation-face ((,class (:inherit font-lock-constant-face))))
;;;;; line numbers (Emacs 26.1 and above)
    `(line-number ((,class (:inherit default :foreground ,zenburn-bg+3 :background ,zenburn-bg-05))))
    `(line-number-current-line ((,class (:inherit line-number :foreground ,zenburn-yellow-2))))
;;;;; man
    `(Man-overstrike ((,class (:inherit font-lock-keyword-face))))
    `(Man-underline  ((,class (:inherit (font-lock-string-face underline)))))
;;;;; newsticker
    `(newsticker-date-face ((,class (:foreground ,zenburn-fg))))
    `(newsticker-default-face ((,class (:foreground ,zenburn-fg))))
    `(newsticker-enclosure-face ((,class (:foreground ,zenburn-green+3))))
    `(newsticker-extra-face ((,class (:foreground ,zenburn-bg+2 :height 0.8))))
    `(newsticker-feed-face ((,class (:foreground ,zenburn-fg))))
    `(newsticker-immortal-item-face ((,class (:foreground ,zenburn-green))))
    `(newsticker-new-item-face ((,class (:foreground ,zenburn-blue))))
    `(newsticker-obsolete-item-face ((,class (:foreground ,zenburn-red))))
    `(newsticker-old-item-face ((,class (:foreground ,zenburn-bg+3))))
    `(newsticker-statistics-face ((,class (:foreground ,zenburn-fg))))
    `(newsticker-treeview-face ((,class (:foreground ,zenburn-fg))))
    `(newsticker-treeview-immortal-face ((,class (:foreground ,zenburn-green))))
    `(newsticker-treeview-listwindow-face ((,class (:foreground ,zenburn-fg))))
    `(newsticker-treeview-new-face ((,class (:foreground ,zenburn-blue :weight bold))))
    `(newsticker-treeview-obsolete-face ((,class (:foreground ,zenburn-red))))
    `(newsticker-treeview-old-face ((,class (:foreground ,zenburn-bg+3))))
    `(newsticker-treeview-selection-face ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-yellow))))
;;;;; woman
    `(woman-bold   ((,class (:inherit font-lock-keyword-face))))
    `(woman-italic ((,class (:inherit (font-lock-string-face italic)))))

;;;; Third-party packages

;;;;; wgrep
    `(wgrep-grep-face ((,class (:inherit isearch))))
;;;;; debbugs
    `(debbugs-gnu-done ((,class (:foreground ,zenburn-fg-1))))
    `(debbugs-gnu-handled ((,class (:foreground ,zenburn-green))))
    `(debbugs-gnu-new ((,class (:foreground ,zenburn-red))))
    `(debbugs-gnu-pending ((,class (:foreground ,zenburn-blue))))
    `(debbugs-gnu-stale ((,class (:foreground ,zenburn-orange))))
    `(debbugs-gnu-tagged ((,class (:foreground ,zenburn-red))))
;;;;; diff
    ;; Please read (info "(magit)Theming Faces") before changing this.
    `(diff-added          ((,class (:background "#335533" :foreground ,zenburn-green))))
    `(diff-changed        ((,class (:background "#555511" :foreground ,zenburn-yellow-1))))
    `(diff-removed        ((,class (:background "#553333" :foreground ,zenburn-red-2))))
    `(diff-refine-added   ((,class (:background "#338833" :foreground ,zenburn-green+4))))
    `(diff-refine-changed ((,class (:background "#888811" :foreground ,zenburn-yellow))))
    `(diff-refine-removed ((,class (:background "#883333" :foreground ,zenburn-red))))
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
    `(dim-autoload-cookie-line ((,class :foreground ,zenburn-bg+1)))
;;;;; dired+
    `(diredp-display-msg ((,class (:foreground ,zenburn-blue))))
    `(diredp-compressed-file-suffix ((,class (:foreground ,zenburn-orange))))
    `(diredp-date-time ((,class (:foreground ,zenburn-magenta))))
    `(diredp-deletion ((,class (:foreground ,zenburn-yellow))))
    `(diredp-deletion-file-name ((,class (:foreground ,zenburn-red))))
    `(diredp-dir-heading ((,class (:foreground ,zenburn-blue :background ,zenburn-bg-1))))
    `(diredp-dir-priv ((,class (:foreground ,zenburn-cyan))))
    `(diredp-exec-priv ((,class (:foreground ,zenburn-red))))
    `(diredp-executable-tag ((,class (:foreground ,zenburn-green+1))))
    `(diredp-file-name ((,class (:foreground ,zenburn-blue))))
    `(diredp-file-suffix ((,class (:foreground ,zenburn-green))))
    `(diredp-flag-mark ((,class (:foreground ,zenburn-yellow))))
    `(diredp-flag-mark-line ((,class (:foreground ,zenburn-orange))))
    `(diredp-ignored-file-name ((,class (:foreground ,zenburn-red))))
    `(diredp-link-priv ((,class (:foreground ,zenburn-yellow))))
    `(diredp-mode-line-flagged ((,class (:foreground ,zenburn-yellow))))
    `(diredp-mode-line-marked ((,class (:foreground ,zenburn-orange))))
    `(diredp-no-priv ((,class (:foreground ,zenburn-fg))))
    `(diredp-number ((,class (:foreground ,zenburn-green+1))))
    `(diredp-other-priv ((,class (:foreground ,zenburn-yellow-1))))
    `(diredp-rare-priv ((,class (:foreground ,zenburn-red-1))))
    `(diredp-read-priv ((,class (:foreground ,zenburn-green-2))))
    `(diredp-symlink ((,class (:foreground ,zenburn-yellow))))
    `(diredp-write-priv ((,class (:foreground ,zenburn-magenta))))
;;;;; dired-async
    `(dired-async-failures ((,class (:foreground ,zenburn-red :weight bold))))
    `(dired-async-message ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(dired-async-mode-message ((,class (:foreground ,zenburn-yellow))))
;;;;; diredfl
    `(diredfl-compressed-file-suffix ((,class (:foreground ,zenburn-orange))))
    `(diredfl-date-time ((,class (:foreground ,zenburn-magenta))))
    `(diredfl-deletion ((,class (:foreground ,zenburn-yellow))))
    `(diredfl-deletion-file-name ((,class (:foreground ,zenburn-red))))
    `(diredfl-dir-heading ((,class (:foreground ,zenburn-blue :background ,zenburn-bg-1))))
    `(diredfl-dir-priv ((,class (:foreground ,zenburn-cyan))))
    `(diredfl-exec-priv ((,class (:foreground ,zenburn-red))))
    `(diredfl-executable-tag ((,class (:foreground ,zenburn-green+1))))
    `(diredfl-file-name ((,class (:foreground ,zenburn-blue))))
    `(diredfl-file-suffix ((,class (:foreground ,zenburn-green))))
    `(diredfl-flag-mark ((,class (:foreground ,zenburn-yellow))))
    `(diredfl-flag-mark-line ((,class (:foreground ,zenburn-orange))))
    `(diredfl-ignored-file-name ((,class (:foreground ,zenburn-red))))
    `(diredfl-link-priv ((,class (:foreground ,zenburn-yellow))))
    `(diredfl-no-priv ((,class (:foreground ,zenburn-fg))))
    `(diredfl-number ((,class (:foreground ,zenburn-green+1))))
    `(diredfl-other-priv ((,class (:foreground ,zenburn-yellow-1))))
    `(diredfl-rare-priv ((,class (:foreground ,zenburn-red-1))))
    `(diredfl-read-priv ((,class (:foreground ,zenburn-green-1))))
    `(diredfl-symlink ((,class (:foreground ,zenburn-yellow))))
    `(diredfl-write-priv ((,class (:foreground ,zenburn-magenta))))
;;;;; ediff
    `(ediff-current-diff-A ((,class (:foreground ,zenburn-fg :background ,zenburn-red-4))))
    `(ediff-current-diff-Ancestor ((,class (:foreground ,zenburn-fg :background ,zenburn-red-4))))
    `(ediff-current-diff-B ((,class (:foreground ,zenburn-fg :background ,zenburn-green-2))))
    `(ediff-current-diff-C ((,class (:foreground ,zenburn-fg :background ,zenburn-blue-5))))
    `(ediff-even-diff-A ((,class (:background ,zenburn-bg+1))))
    `(ediff-even-diff-Ancestor ((,class (:background ,zenburn-bg+1))))
    `(ediff-even-diff-B ((,class (:background ,zenburn-bg+1))))
    `(ediff-even-diff-C ((,class (:background ,zenburn-bg+1))))
    `(ediff-fine-diff-A ((,class (:foreground ,zenburn-fg :background ,zenburn-red-2 :weight bold))))
    `(ediff-fine-diff-Ancestor ((,class (:foreground ,zenburn-fg :background ,zenburn-red-2 weight bold))))
    `(ediff-fine-diff-B ((,class (:foreground ,zenburn-fg :background ,zenburn-green :weight bold))))
    `(ediff-fine-diff-C ((,class (:foreground ,zenburn-fg :background ,zenburn-blue-3 :weight bold ))))
    `(ediff-odd-diff-A ((,class (:background ,zenburn-bg+2))))
    `(ediff-odd-diff-Ancestor ((,class (:background ,zenburn-bg+2))))
    `(ediff-odd-diff-B ((,class (:background ,zenburn-bg+2))))
    `(ediff-odd-diff-C ((,class (:background ,zenburn-bg+2))))
;;;;; emacs-w3m
    `(w3m-anchor ((,class (:foreground ,zenburn-yellow :underline t
                                      :weight bold))))
    `(w3m-arrived-anchor ((,class (:foreground ,zenburn-yellow-2
                                              :underline t :weight normal))))
    `(w3m-form ((,class (:foreground ,zenburn-red-1 :underline t))))
    `(w3m-header-line-location-title ((,class (:foreground ,zenburn-yellow
                                                          :underline t :weight bold))))
    `(w3m-history-current-url ((,class (:inherit match))))
    `(w3m-lnum ((,class (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
    `(w3m-lnum-match ((,class (:background ,zenburn-bg-1
                                          :foreground ,zenburn-orange
                                          :weight bold))))
    `(w3m-lnum-minibuffer-prompt ((,class (:foreground ,zenburn-yellow))))
;;;;; eshell
    `(eshell-prompt ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(eshell-ls-archive ((,class (:foreground ,zenburn-red-1 :weight bold))))
    `(eshell-ls-backup ((,class (:inherit font-lock-comment-face))))
    `(eshell-ls-clutter ((,class (:inherit font-lock-comment-face))))
    `(eshell-ls-directory ((,class (:foreground ,zenburn-blue+1 :weight bold))))
    `(eshell-ls-executable ((,class (:foreground ,zenburn-red+1 :weight bold))))
    `(eshell-ls-unreadable ((,class (:foreground ,zenburn-fg))))
    `(eshell-ls-missing ((,class (:inherit font-lock-warning-face))))
    `(eshell-ls-product ((,class (:inherit font-lock-doc-face))))
    `(eshell-ls-special ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(eshell-ls-symlink ((,class (:foreground ,zenburn-cyan :weight bold))))
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
    `(flycheck-fringe-error ((,class (:foreground ,zenburn-red-1 :weight bold))))
    `(flycheck-fringe-warning ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(flycheck-fringe-info ((,class (:foreground ,zenburn-cyan :weight bold))))
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
    `(git-annex-dired-annexed-available ((,class (:inherit success :weight normal))))
    `(git-annex-dired-annexed-unavailable ((,class (:inherit error :weight normal))))
;;;;; git-commit
    `(git-commit-comment-action  ((,class (:foreground ,zenburn-green+1 :weight bold))))
    `(git-commit-comment-branch  ((,class (:foreground ,zenburn-blue+1  :weight bold)))) ; obsolete
    `(git-commit-comment-branch-local  ((,class (:foreground ,zenburn-blue+1  :weight bold))))
    `(git-commit-comment-branch-remote ((,class (:foreground ,zenburn-green  :weight bold))))
    `(git-commit-comment-heading ((,class (:foreground ,zenburn-yellow  :weight bold))))
;;;;; git-gutter
    `(git-gutter:added ((,class (:foreground ,zenburn-green :weight bold :inverse-video t))))
    `(git-gutter:deleted ((,class (:foreground ,zenburn-red :weight bold :inverse-video t))))
    `(git-gutter:modified ((,class (:foreground ,zenburn-magenta :weight bold :inverse-video t))))
    `(git-gutter:unchanged ((,class (:foreground ,zenburn-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
    `(git-gutter-fr:added ((,class (:foreground ,zenburn-green  :weight bold))))
    `(git-gutter-fr:deleted ((,class (:foreground ,zenburn-red :weight bold))))
    `(git-gutter-fr:modified ((,class (:foreground ,zenburn-magenta :weight bold))))
;;;;; git-rebase
    `(git-rebase-hash ((,class (:foreground, zenburn-orange))))
;;;;; gnus
    `(gnus-group-mail-1 ((,class (:weight bold :inherit gnus-group-mail-1-empty))))
    `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-news-1-empty))))
    `(gnus-group-mail-2 ((,class (:weight bold :inherit gnus-group-mail-2-empty))))
    `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-news-2-empty))))
    `(gnus-group-mail-3 ((,class (:weight bold :inherit gnus-group-mail-3-empty))))
    `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-news-3-empty))))
    `(gnus-group-mail-4 ((,class (:weight bold :inherit gnus-group-mail-4-empty))))
    `(gnus-group-mail-4-empty ((,class (:inherit gnus-group-news-4-empty))))
    `(gnus-group-mail-5 ((,class (:weight bold :inherit gnus-group-mail-5-empty))))
    `(gnus-group-mail-5-empty ((,class (:inherit gnus-group-news-5-empty))))
    `(gnus-group-mail-6 ((,class (:weight bold :inherit gnus-group-mail-6-empty))))
    `(gnus-group-mail-6-empty ((,class (:inherit gnus-group-news-6-empty))))
    `(gnus-group-mail-low ((,class (:weight bold :inherit gnus-group-mail-low-empty))))
    `(gnus-group-mail-low-empty ((,class (:inherit gnus-group-news-low-empty))))
    `(gnus-group-news-1 ((,class (:weight bold :inherit gnus-group-news-1-empty))))
    `(gnus-group-news-2 ((,class (:weight bold :inherit gnus-group-news-2-empty))))
    `(gnus-group-news-3 ((,class (:weight bold :inherit gnus-group-news-3-empty))))
    `(gnus-group-news-4 ((,class (:weight bold :inherit gnus-group-news-4-empty))))
    `(gnus-group-news-5 ((,class (:weight bold :inherit gnus-group-news-5-empty))))
    `(gnus-group-news-6 ((,class (:weight bold :inherit gnus-group-news-6-empty))))
    `(gnus-group-news-low ((,class (:weight bold :inherit gnus-group-news-low-empty))))
    `(gnus-header-content ((,class (:inherit message-header-other))))
    `(gnus-header-from ((,class (:inherit message-header-to))))
    `(gnus-header-name ((,class (:inherit message-header-name))))
    `(gnus-header-newsgroups ((,class (:inherit message-header-other))))
    `(gnus-header-subject ((,class (:inherit message-header-subject))))
    `(gnus-server-opened ((,class (:foreground ,zenburn-green+2 :weight bold))))
    `(gnus-server-denied ((,class (:foreground ,zenburn-red+1 :weight bold))))
    `(gnus-server-closed ((,class (:foreground ,zenburn-blue :slant italic))))
    `(gnus-server-offline ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(gnus-server-agent ((,class (:foreground ,zenburn-blue :weight bold))))
    `(gnus-summary-cancelled ((,class (:foreground ,zenburn-orange))))
    `(gnus-summary-high-ancient ((,class (:foreground ,zenburn-blue))))
    `(gnus-summary-high-read ((,class (:foreground ,zenburn-green :weight bold))))
    `(gnus-summary-high-ticked ((,class (:foreground ,zenburn-orange :weight bold))))
    `(gnus-summary-high-unread ((,class (:foreground ,zenburn-fg :weight bold))))
    `(gnus-summary-low-ancient ((,class (:foreground ,zenburn-blue))))
    `(gnus-summary-low-read ((,class (:foreground ,zenburn-green))))
    `(gnus-summary-low-ticked ((,class (:foreground ,zenburn-orange :weight bold))))
    `(gnus-summary-low-unread ((,class (:foreground ,zenburn-fg))))
    `(gnus-summary-normal-ancient ((,class (:foreground ,zenburn-blue))))
    `(gnus-summary-normal-read ((,class (:foreground ,zenburn-green))))
    `(gnus-summary-normal-ticked ((,class (:foreground ,zenburn-orange :weight bold))))
    `(gnus-summary-normal-unread ((,class (:foreground ,zenburn-fg))))
    `(gnus-summary-selected ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(gnus-cite-1 ((,class (:foreground ,zenburn-blue))))
    `(gnus-cite-10 ((,class (:foreground ,zenburn-yellow-1))))
    `(gnus-cite-11 ((,class (:foreground ,zenburn-yellow))))
    `(gnus-cite-2 ((,class (:foreground ,zenburn-blue-1))))
    `(gnus-cite-3 ((,class (:foreground ,zenburn-blue-2))))
    `(gnus-cite-4 ((,class (:foreground ,zenburn-green+2))))
    `(gnus-cite-5 ((,class (:foreground ,zenburn-green+1))))
    `(gnus-cite-6 ((,class (:foreground ,zenburn-green))))
    `(gnus-cite-7 ((,class (:foreground ,zenburn-red))))
    `(gnus-cite-8 ((,class (:foreground ,zenburn-red-1))))
    `(gnus-cite-9 ((,class (:foreground ,zenburn-red-2))))
    `(gnus-group-news-1-empty ((,class (:foreground ,zenburn-yellow))))
    `(gnus-group-news-2-empty ((,class (:foreground ,zenburn-green+3))))
    `(gnus-group-news-3-empty ((,class (:foreground ,zenburn-green+1))))
    `(gnus-group-news-4-empty ((,class (:foreground ,zenburn-blue-2))))
    `(gnus-group-news-5-empty ((,class (:foreground ,zenburn-blue-3))))
    `(gnus-group-news-6-empty ((,class (:foreground ,zenburn-bg+2))))
    `(gnus-group-news-low-empty ((,class (:foreground ,zenburn-bg+2))))
    `(gnus-signature ((,class (:foreground ,zenburn-yellow))))
    `(gnus-x ((,class (:background ,zenburn-fg :foreground ,zenburn-bg))))
    `(mm-uu-extract ((,class (:background ,zenburn-bg-05 :foreground ,zenburn-green+1))))
;;;;; highlight-numbers
    `(highlight-numbers-number ((,class (:foreground ,zenburn-blue))))
;;;;; highlight-symbol
    `(highlight-symbol-face ((,class (:background ,zenburn-bg+2))))
;;;;; highlight-thing
    `(highlight-thing ((,class (:background ,zenburn-bg+2))))
;;;;; hl-line-mode
    `(hl-line-face ((,class (:background ,zenburn-bg-05))
                     (t :weight bold)))
    `(hl-line ((,class (:background ,zenburn-bg-05 :extend t)) ; old emacsen
                (t :weight bold)))
;;;;; hl-sexp
    `(hl-sexp-face ((,class (:background ,zenburn-bg+1))
                     (t :weight bold)))
;;;;; corfu
    `(corfu-default ((,class (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
    `(corfu-current ((,class (:foreground ,zenburn-fg :background ,zenburn-bg-1))))
    `(corfu-bar ((,class (:background unspecified :foreground unspecified))))
    `(corfu-border ((,class (:background ,zenburn-bg+1))))
;;;;; ido-mode
    `(ido-first-match ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(ido-only-match ((,class (:foreground ,zenburn-orange :weight bold))))
    `(ido-subdir ((,class (:foreground ,zenburn-yellow))))
    `(ido-indicator ((,class (:foreground ,zenburn-yellow :background ,zenburn-red-4))))
;;;;; iedit-mode
    `(iedit-occurrence ((,class (:background ,zenburn-bg+2 :weight bold))))
;;;; js2-mode
    `(js2-warning ((,class (:underline ,zenburn-orange))))
    `(js2-error ((,class (:foreground ,zenburn-red :weight bold))))
    `(js2-jsdoc-tag ((,class (:foreground ,zenburn-green-2))))
    `(js2-jsdoc-type ((,class (:foreground ,zenburn-green+2))))
    `(js2-jsdoc-value ((,class (:foreground ,zenburn-green+3))))
    `(js2-function-param ((,class (:foreground, zenburn-orange))))
    `(js2-external-variable ((,class (:foreground ,zenburn-orange))))
;;;;; additional js2 mode attributes for better syntax highlighting
    `(js2-instance-member ((,class (:foreground ,zenburn-green-2))))
    `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,zenburn-orange))))
    `(js2-jsdoc-html-tag-name ((,class (:foreground ,zenburn-red-1))))
    `(js2-object-property ((,class (:foreground ,zenburn-blue+1))))
    `(js2-magic-paren ((,class (:foreground ,zenburn-blue-5))))
    `(js2-private-function-call ((,class (:foreground ,zenburn-cyan))))
    `(js2-function-call ((,class (:foreground ,zenburn-cyan))))
    `(js2-private-member ((,class (:foreground ,zenburn-blue-1))))
    `(js2-keywords ((,class (:foreground ,zenburn-magenta))))
;;;;; linum-mode
    `(linum ((,class (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
;;;;; lispy
    `(lispy-command-name-face ((,class (:background ,zenburn-bg-05 :inherit font-lock-function-name-face))))
    `(lispy-cursor-face ((,class (:foreground ,zenburn-bg :background ,zenburn-fg))))
    `(lispy-face-hint ((,class (:inherit highlight :foreground ,zenburn-yellow))))
;;;;; ruler-mode
    `(ruler-mode-column-number ((,class (:inherit 'ruler-mode-default :foreground ,zenburn-fg))))
    `(ruler-mode-fill-column ((,class (:inherit 'ruler-mode-default :foreground ,zenburn-yellow))))
    `(ruler-mode-goal-column ((,class (:inherit 'ruler-mode-fill-column))))
    `(ruler-mode-comment-column ((,class (:inherit 'ruler-mode-fill-column))))
    `(ruler-mode-tab-stop ((,class (:inherit 'ruler-mode-fill-column))))
    `(ruler-mode-current-column ((,class (:foreground ,zenburn-yellow :box t))))
    `(ruler-mode-default ((,class (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
;;;;; magit
;;;;;; headings and diffs
    ;; Please read (info "(magit)Theming Faces") before changing this.
    `(magit-section-highlight           ((,class (:background ,zenburn-bg+05))))
    `(magit-section-heading             ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(magit-section-heading-selection   ((,class (:foreground ,zenburn-orange :weight bold))))
    `(magit-diff-file-heading           ((,class (:weight bold))))
    `(magit-diff-file-heading-highlight ((,class (:background ,zenburn-bg+05 :weight bold))))
    `(magit-diff-file-heading-selection ((,class (:background ,zenburn-bg+05 :weight bold
                                                             :foreground ,zenburn-orange))))
    `(magit-diff-added                  ((,class (:background ,zenburn-green-2))))
    `(magit-diff-added-highlight        ((,class (:background ,zenburn-green))))
    `(magit-diff-removed                ((,class (:background ,zenburn-red-4))))
    `(magit-diff-removed-highlight      ((,class (:background ,zenburn-red-3))))
    `(magit-diff-hunk-heading           ((,class (:background ,zenburn-bg+1))))
    `(magit-diff-hunk-heading-highlight ((,class (:background ,zenburn-bg+2))))
    `(magit-diff-hunk-heading-selection ((,class (:background ,zenburn-bg+2
                                                             :foreground ,zenburn-orange))))
    `(magit-diff-lines-heading          ((,class (:background ,zenburn-orange
                                                             :foreground ,zenburn-bg+2))))
    `(magit-diff-context-highlight      ((,class (:background ,zenburn-bg+05
                                                             :foreground "grey70"))))
    `(magit-diffstat-added              ((,class (:foreground ,zenburn-green+4))))
    `(magit-diffstat-removed            ((,class (:foreground ,zenburn-red))))
;;;;;; popup
    `(magit-popup-heading             ((,class (:foreground ,zenburn-yellow  :weight bold))))
    `(magit-popup-key                 ((,class (:foreground ,zenburn-green-2 :weight bold))))
    `(magit-popup-argument            ((,class (:foreground ,zenburn-green   :weight bold))))
    `(magit-popup-disabled-argument   ((,class (:foreground ,zenburn-fg-1    :weight normal))))
    `(magit-popup-option-value        ((,class (:foreground ,zenburn-blue-2  :weight bold))))
;;;;;; process
    `(magit-process-ok    ((,class (:foreground ,zenburn-green  :weight bold))))
    `(magit-process-ng    ((,class (:foreground ,zenburn-red    :weight bold))))
;;;;;; log
    `(magit-log-author    ((,class (:foreground ,zenburn-orange))))
    `(magit-log-date      ((,class (:foreground ,zenburn-fg-1))))
    `(magit-log-graph     ((,class (:foreground ,zenburn-fg+1))))
;;;;;; sequence
    `(magit-sequence-pick ((,class (:foreground ,zenburn-yellow-2))))
    `(magit-sequence-stop ((,class (:foreground ,zenburn-green))))
    `(magit-sequence-part ((,class (:foreground ,zenburn-yellow))))
    `(magit-sequence-head ((,class (:foreground ,zenburn-blue))))
    `(magit-sequence-drop ((,class (:foreground ,zenburn-red))))
    `(magit-sequence-done ((,class (:foreground ,zenburn-fg-1))))
    `(magit-sequence-onto ((,class (:foreground ,zenburn-fg-1))))
;;;;;; bisect
    `(magit-bisect-good ((,class (:foreground ,zenburn-green))))
    `(magit-bisect-skip ((,class (:foreground ,zenburn-yellow))))
    `(magit-bisect-bad  ((,class (:foreground ,zenburn-red))))
;;;;;; blame
    `(magit-blame-heading ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2))))
    `(magit-blame-hash    ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2))))
    `(magit-blame-name    ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))
    `(magit-blame-date    ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))
    `(magit-blame-summary ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2
                                               :weight bold))))
;;;;;; references etc
    `(magit-dimmed         ((,class (:foreground ,zenburn-bg+3))))
    `(magit-hash           ((,class (:foreground ,zenburn-bg+3))))
    `(magit-tag            ((,class (:foreground ,zenburn-orange :weight bold))))
    `(magit-branch-remote  ((,class (:foreground ,zenburn-green  :weight bold))))
    `(magit-branch-local   ((,class (:foreground ,zenburn-blue   :weight bold))))
    `(magit-branch-current ((,class (:foreground ,zenburn-blue   :weight bold :box t))))
    `(magit-head           ((,class (:foreground ,zenburn-blue   :weight bold))))
    `(magit-refname        ((,class (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
    `(magit-refname-stash  ((,class (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
    `(magit-refname-wip    ((,class (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
    `(magit-signature-good      ((,class (:foreground ,zenburn-green))))
    `(magit-signature-bad       ((,class (:foreground ,zenburn-red))))
    `(magit-signature-untrusted ((,class (:foreground ,zenburn-yellow))))
    `(magit-signature-expired   ((,class (:foreground ,zenburn-orange))))
    `(magit-signature-revoked   ((,class (:foreground ,zenburn-magenta))))
    `(magit-signature-error     ((,class (:inherit    magit-signature-bad))))
    `(magit-cherry-unmatched    ((,class (:foreground ,zenburn-cyan))))
    `(magit-cherry-equivalent   ((,class (:foreground ,zenburn-magenta))))
    `(magit-reflog-commit       ((,class (:foreground ,zenburn-green))))
    `(magit-reflog-amend        ((,class (:foreground ,zenburn-magenta))))
    `(magit-reflog-merge        ((,class (:foreground ,zenburn-green))))
    `(magit-reflog-checkout     ((,class (:foreground ,zenburn-blue))))
    `(magit-reflog-reset        ((,class (:foreground ,zenburn-red))))
    `(magit-reflog-rebase       ((,class (:foreground ,zenburn-magenta))))
    `(magit-reflog-cherry-pick  ((,class (:foreground ,zenburn-green))))
    `(magit-reflog-remote       ((,class (:foreground ,zenburn-cyan))))
    `(magit-reflog-other        ((,class (:foreground ,zenburn-cyan))))
;;;;; markup-faces
    `(markup-anchor-face ((,class (:foreground ,zenburn-blue+1))))
    `(markup-code-face ((,class (:inherit font-lock-constant-face))))
    `(markup-command-face ((,class (:foreground ,zenburn-yellow))))
    `(markup-emphasis-face ((,class (:inherit bold))))
    `(markup-internal-reference-face ((,class (:foreground ,zenburn-yellow-2 :underline t))))
    `(markup-list-face ((,class (:foreground ,zenburn-fg+1))))
    `(markup-meta-face ((,class (:foreground ,zenburn-yellow))))
    `(markup-meta-hide-face ((,class (:foreground ,zenburn-yellow))))
    `(markup-secondary-text-face ((,class (:foreground ,zenburn-yellow-1))))
    `(markup-title-0-face ((,class (:inherit font-lock-function-name-face :weight bold))))
    `(markup-title-1-face ((,class (:inherit font-lock-function-name-face :weight bold))))
    `(markup-title-2-face ((,class (:inherit font-lock-function-name-face :weight bold))))
    `(markup-title-3-face ((,class (:inherit font-lock-function-name-face :weight bold))))
    `(markup-title-4-face ((,class (:inherit font-lock-function-name-face :weight bold))))
    `(markup-typewriter-face ((,class (:inherit font-lock-constant-face))))
    `(markup-verbatim-face ((,class (:inherit font-lock-constant-face))))
    `(markup-value-face ((,class (:foreground ,zenburn-yellow))))
;;;;; message-mode
    `(message-cited-text ((,class (:inherit font-lock-comment-face))))
    `(message-header-name ((,class (:foreground ,zenburn-green+1))))
    `(message-header-other ((,class (:foreground ,zenburn-green))))
    `(message-header-to ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(message-header-cc ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(message-header-newsgroups ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(message-header-subject ((,class (:foreground ,zenburn-orange :weight bold))))
    `(message-header-xheader ((,class (:foreground ,zenburn-green))))
    `(message-mml ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(message-separator ((,class (:inherit font-lock-comment-face))))
;;;;; mu4e
    `(mu4e-cited-1-face ((,class (:foreground ,zenburn-blue    :slant italic))))
    `(mu4e-cited-2-face ((,class (:foreground ,zenburn-green+2 :slant italic))))
    `(mu4e-cited-3-face ((,class (:foreground ,zenburn-blue-2  :slant italic))))
    `(mu4e-cited-4-face ((,class (:foreground ,zenburn-green   :slant italic))))
    `(mu4e-cited-5-face ((,class (:foreground ,zenburn-blue-4  :slant italic))))
    `(mu4e-cited-6-face ((,class (:foreground ,zenburn-green-2 :slant italic))))
    `(mu4e-cited-7-face ((,class (:foreground ,zenburn-blue    :slant italic))))
    `(mu4e-replied-face ((,class (:foreground ,zenburn-bg+3))))
    `(mu4e-trashed-face ((,class (:foreground ,zenburn-bg+3 :strike-through t))))
;;;;; orderless
    `(orderless-match-face-0 ((,class (:foreground ,zenburn-green))))
    `(orderless-match-face-1 ((,class (:foreground ,zenburn-magenta))))
    `(orderless-match-face-2 ((,class (:foreground ,zenburn-blue))))
    `(orderless-match-face-3 ((,class (:foreground ,zenburn-orange))))
;;;;; org-mode
    `(org-agenda-date-today
       ((,class (:foreground ,zenburn-fg+1 :slant italic :weight bold))) t)
    `(org-agenda-structure
       ((,class (:inherit font-lock-comment-face))))
    `(org-archived ((,class (:foreground ,zenburn-fg :weight bold))))
    `(org-block ((,class (:background ,zenburn-bg+05 :extend t))))
    `(org-checkbox ((,class (:background ,zenburn-bg+2 :foreground ,zenburn-fg+1
                                        :box (:line-width 1 :style released-button)))))
    `(org-date ((,class (:foreground ,zenburn-blue :underline t))))
    `(org-deadline-announce ((,class (:foreground ,zenburn-red-1))))
    `(org-done ((,class (:weight bold :weight bold :foreground ,zenburn-green+3))))
    `(org-formula ((,class (:foreground ,zenburn-yellow-2))))
    `(org-headline-done ((,class (:foreground ,zenburn-green+3))))
    `(org-hide ((,class (:foreground ,zenburn-bg))))
    `(org-level-1 ((,class (:inherit ,z-variable-pitch :foreground ,zenburn-orange
                                       ,@(when zenburn-scale-org-headlines
                                           (list :height zenburn-height-plus-4))))))
    `(org-level-2 ((,class (:inherit ,z-variable-pitch :foreground ,zenburn-green+4
                                       ,@(when zenburn-scale-org-headlines
                                           (list :height zenburn-height-plus-3))))))
    `(org-level-3 ((,class (:inherit ,z-variable-pitch :foreground ,zenburn-blue-1
                                       ,@(when zenburn-scale-org-headlines
                                           (list :height zenburn-height-plus-2))))))
    `(org-level-4 ((,class (:inherit ,z-variable-pitch :foreground ,zenburn-yellow-2
                                       ,@(when zenburn-scale-org-headlines
                                           (list :height zenburn-height-plus-1))))))
    `(org-level-5 ((,class (:inherit ,z-variable-pitch :foreground ,zenburn-cyan))))
    `(org-level-6 ((,class (:inherit ,z-variable-pitch :foreground ,zenburn-green+2))))
    `(org-level-7 ((,class (:inherit ,z-variable-pitch :foreground ,zenburn-red+2))))
    `(org-level-8 ((,class (:inherit ,z-variable-pitch :foreground ,zenburn-magenta))))
    `(org-link ((,class (:foreground ,zenburn-yellow-2 :underline t))))
    `(org-quote ((,class (:background ,zenburn-bg+05 :extend t))))
    `(org-scheduled ((,class (:foreground ,zenburn-green+4))))
    `(org-scheduled-previously ((,class (:foreground ,zenburn-red))))
    `(org-scheduled-today ((,class (:foreground ,zenburn-blue+1))))
    `(org-sexp-date ((,class (:foreground ,zenburn-blue+1 :underline t))))
    `(org-special-keyword ((,class (:inherit font-lock-comment-face))))
    `(org-table ((,class (:foreground ,zenburn-green+2))))
    `(org-tag ((,class (:weight bold :weight bold))))
    `(org-time-grid ((,class (:foreground ,zenburn-orange))))
    `(org-todo ((,class (:weight bold :foreground ,zenburn-red :weight bold))))
    `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
    `(org-warning ((,class (:weight bold :foreground ,zenburn-red :weight bold :underline nil))))
    `(org-column ((,class (:background ,zenburn-bg-1))))
    `(org-column-title ((,class (:background ,zenburn-bg-1 :underline t :weight bold))))
    `(org-mode-line-clock ((,class (:foreground ,zenburn-fg :background ,zenburn-bg-1))))
    `(org-mode-line-clock-overrun ((,class (:foreground ,zenburn-bg :background ,zenburn-red-1))))
    `(org-ellipsis ((,class (:foreground ,zenburn-yellow-1 :underline t))))
    `(org-footnote ((,class (:foreground ,zenburn-cyan :underline t))))
    `(org-document-title ((,class (:inherit ,z-variable-pitch :foreground ,zenburn-blue
                                              :weight bold
                                              ,@(when zenburn-scale-org-headlines
                                                  (list :height zenburn-height-plus-4))))))
    `(org-document-info ((,class (:foreground ,zenburn-blue))))
    `(org-habit-ready-face ((,class :background ,zenburn-green)))
    `(org-habit-alert-face ((,class :background ,zenburn-yellow-1 :foreground ,zenburn-bg)))
    `(org-habit-clear-face ((,class :background ,zenburn-blue-3)))
    `(org-habit-overdue-face ((,class :background ,zenburn-red-3)))
    `(org-habit-clear-future-face ((,class :background ,zenburn-blue-4)))
    `(org-habit-ready-future-face ((,class :background ,zenburn-green-2)))
    `(org-habit-alert-future-face ((,class :background ,zenburn-yellow-2 :foreground ,zenburn-bg)))
    `(org-habit-overdue-future-face ((,class :background ,zenburn-red-4)))
;;;;; org-ref
    `(org-ref-ref-face ((,class :underline t)))
    `(org-ref-label-face ((,class :underline t)))
    `(org-ref-cite-face ((,class :underline t)))
    `(org-ref-glossary-face ((,class :underline t)))
    `(org-ref-acronym-face ((,class :underline t)))
;;;;; outline
    `(outline-1 ((,class (:inherit ,z-variable-pitch :foreground ,zenburn-orange
                                     ,@(when zenburn-scale-outline-headlines
                                         (list :height zenburn-height-plus-4))))))
    `(outline-2 ((,class (:inherit ,z-variable-pitch :foreground ,zenburn-green+4
                                     ,@(when zenburn-scale-outline-headlines
                                         (list :height zenburn-height-plus-3))))))
    `(outline-3 ((,class (:inherit ,z-variable-pitch :foreground ,zenburn-blue-1
                                     ,@(when zenburn-scale-outline-headlines
                                         (list :height zenburn-height-plus-2))))))
    `(outline-4 ((,class (:inherit ,z-variable-pitch :foreground ,zenburn-yellow-2
                                     ,@(when zenburn-scale-outline-headlines
                                         (list :height zenburn-height-plus-1))))))
    `(outline-5 ((,class (:inherit ,z-variable-pitch :foreground ,zenburn-cyan))))
    `(outline-6 ((,class (:inherit ,z-variable-pitch :foreground ,zenburn-green+2))))
    `(outline-7 ((,class (:inherit ,z-variable-pitch :foreground ,zenburn-red-4))))
    `(outline-8 ((,class (:inherit ,z-variable-pitch :foreground ,zenburn-blue-4))))
;;;;; c/perl
    `(cperl-nonoverridable-face ((,class (:foreground ,zenburn-magenta))))
    `(cperl-array-face ((,class (:foreground ,zenburn-yellow, :background ,zenburn-bg))))
    `(cperl-hash-face ((,class (:foreground ,zenburn-yellow-1, :background ,zenburn-bg))))
;;;;; paren-face
    `(parenthesis ((,class (:foreground ,zenburn-fg-1))))
;;;;; perspective
    `(persp-selected-face ((,class (:foreground ,zenburn-yellow-2))))
;;;;; powerline
    `(powerline-active1 ((,class (:background ,zenburn-bg-05 :inherit mode-line))))
    `(powerline-active2 ((,class (:background ,zenburn-bg+2 :inherit mode-line))))
    `(powerline-inactive1 ((,class (:background ,zenburn-bg+1 :inherit mode-line-inactive))))
    `(powerline-inactive2 ((,class (:background ,zenburn-bg+3 :inherit mode-line-inactive))))
;;;;; re-builder
    `(reb-match-0 ((,class (:foreground ,zenburn-bg :background ,zenburn-magenta))))
    `(reb-match-1 ((,class (:foreground ,zenburn-bg :background ,zenburn-blue))))
    `(reb-match-2 ((,class (:foreground ,zenburn-bg :background ,zenburn-orange))))
    `(reb-match-3 ((,class (:foreground ,zenburn-bg :background ,zenburn-red))))
;;;;; rmail
    `(rmail-highlight ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(rmail-header-name ((,class (:foreground ,zenburn-blue))))
;;;;; rpm-mode
    `(rpm-spec-dir-face ((,class (:foreground ,zenburn-green))))
    `(rpm-spec-doc-face ((,class (:foreground ,zenburn-green))))
    `(rpm-spec-ghost-face ((,class (:foreground ,zenburn-red))))
    `(rpm-spec-macro-face ((,class (:foreground ,zenburn-yellow))))
    `(rpm-spec-obsolete-tag-face ((,class (:foreground ,zenburn-red))))
    `(rpm-spec-package-face ((,class (:foreground ,zenburn-red))))
    `(rpm-spec-section-face ((,class (:foreground ,zenburn-yellow))))
    `(rpm-spec-tag-face ((,class (:foreground ,zenburn-blue))))
    `(rpm-spec-var-face ((,class (:foreground ,zenburn-red))))
;;;;; rst-mode
    `(rst-level-1-face ((,class (:foreground ,zenburn-orange))))
    `(rst-level-2-face ((,class (:foreground ,zenburn-green+1))))
    `(rst-level-3-face ((,class (:foreground ,zenburn-blue-1))))
    `(rst-level-4-face ((,class (:foreground ,zenburn-yellow-2))))
    `(rst-level-5-face ((,class (:foreground ,zenburn-cyan))))
    `(rst-level-6-face ((,class (:foreground ,zenburn-green-2))))
;;;;; selectrum
    `(selectrum-current-candidate ((,class (:foreground ,zenburn-yellow :weight bold :underline t))))
    `(selectrum-primary-highlight ((,class (:background ,zenburn-green-2))))
    `(selectrum-secondary-highlight ((,class (:background ,zenburn-green))))
;;;;; sh-mode
    `(sh-heredoc     ((,class (:foreground ,zenburn-yellow :weight bold))))
    `(sh-quoted-exec ((,class (:foreground ,zenburn-red))))
;;;;; show-paren
    `(show-paren-mismatch ((,class (:foreground ,zenburn-red+1 :background ,zenburn-bg+3 :weight bold))))
    `(show-paren-match ((,class (:foreground ,zenburn-fg :background ,zenburn-bg+3 :weight bold))))
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
    `(sp-show-pair-mismatch-face ((,class (:foreground ,zenburn-red+1 :background ,zenburn-bg+3 :weight bold))))
    `(sp-show-pair-match-face ((,class (:background ,zenburn-bg+3 :weight bold))))
;;;;; sml-mode-line
    `(sml-modeline-end-face ((,class :inherit default :width condensed)))
;;;;; term
    `(term-color-black ((,class (:foreground ,zenburn-bg
                                            :background ,zenburn-bg-1))))
    `(term-color-red ((,class (:foreground ,zenburn-red-2
                                          :background ,zenburn-red-4))))
    `(term-color-green ((,class (:foreground ,zenburn-green
                                            :background ,zenburn-green+2))))
    `(term-color-yellow ((,class (:foreground ,zenburn-orange
                                             :background ,zenburn-yellow))))
    `(term-color-blue ((,class (:foreground ,zenburn-blue-1
                                           :background ,zenburn-blue-4))))
    `(term-color-magenta ((,class (:foreground ,zenburn-magenta
                                              :background ,zenburn-red))))
    `(term-color-cyan ((,class (:foreground ,zenburn-cyan
                                           :background ,zenburn-blue))))
    `(term-color-white ((,class (:foreground ,zenburn-fg
                                            :background ,zenburn-fg-1))))
    `(term-default-fg-color ((,class (:inherit term-color-white))))
    `(term-default-bg-color ((,class (:inherit term-color-black))))
;;;;; undo-tree
    `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,zenburn-fg+1 :weight bold))))
    `(undo-tree-visualizer-current-face ((,class (:foreground ,zenburn-red-1 :weight bold))))
    `(undo-tree-visualizer-default-face ((,class (:foreground ,zenburn-fg))))
    `(undo-tree-visualizer-register-face ((,class (:foreground ,zenburn-yellow))))
    `(undo-tree-visualizer-unmodified-face ((,class (:foreground ,zenburn-cyan))))
;;;;; vertico
    `(vertico-current ((,class (:foreground ,zenburn-yellow :weight bold :underline t))))
;;;;; visual-regexp
    `(vr/group-0 ((,class (:foreground ,zenburn-bg :background ,zenburn-green :weight bold))))
    `(vr/group-1 ((,class (:foreground ,zenburn-bg :background ,zenburn-orange :weight bold))))
    `(vr/group-2 ((,class (:foreground ,zenburn-bg :background ,zenburn-blue :weight bold))))
    `(vr/match-0 ((,class (:inherit isearch))))
    `(vr/match-1 ((,class (:foreground ,zenburn-yellow-2 :background ,zenburn-bg-1 :weight bold))))
    `(vr/match-separator-face ((,class (:foreground ,zenburn-red :weight bold))))
;;;;; volatile-highlights
    `(vhl/default-face ((,class (:background ,zenburn-bg-05))))
;;;;; web-mode
    `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
    `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
    `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
    `(web-mode-css-at-rule-face ((,class (:foreground ,zenburn-orange ))))
    `(web-mode-css-prop-face ((,class (:foreground ,zenburn-orange))))
    `(web-mode-css-pseudo-class-face ((,class (:foreground ,zenburn-green+3 :weight bold))))
    `(web-mode-css-rule-face ((,class (:foreground ,zenburn-blue))))
    `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
    `(web-mode-folded-face ((,class (:underline t))))
    `(web-mode-function-name-face ((,class (:foreground ,zenburn-blue))))
    `(web-mode-html-attr-name-face ((,class (:foreground ,zenburn-orange))))
    `(web-mode-html-attr-value-face ((,class (:inherit ,font-lock-string-face))))
    `(web-mode-html-tag-face ((,class (:foreground ,zenburn-cyan))))
    `(web-mode-keyword-face ((,class (:inherit ,font-lock-keyword-face))))
    `(web-mode-preprocessor-face ((,class (:inherit ,font-lock-preprocessor-face))))
    `(web-mode-string-face ((,class (:inherit ,font-lock-string-face))))
    `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
    `(web-mode-variable-name-face ((,class (:inherit ,font-lock-variable-name-face))))
    `(web-mode-server-background-face ((,class (:background ,zenburn-bg))))
    `(web-mode-server-comment-face ((,class (:inherit web-mode-comment-face))))
    `(web-mode-server-string-face ((,class (:inherit web-mode-string-face))))
    `(web-mode-symbol-face ((,class (:inherit font-lock-constant-face))))
    `(web-mode-warning-face ((,class (:inherit font-lock-warning-face))))
    `(web-mode-whitespaces-face ((,class (:background ,zenburn-red))))
;;;;; whitespace-mode
    `(whitespace-space ((,class (:background ,zenburn-bg+1 :foreground ,zenburn-bg+1))))
    `(whitespace-hspace ((,class (:background ,zenburn-bg+1 :foreground ,zenburn-bg+1))))
    `(whitespace-tab ((,class (:background ,zenburn-red-1))))
    `(whitespace-newline ((,class (:foreground ,zenburn-bg+1))))
    `(whitespace-trailing ((,class (:background ,zenburn-red))))
    `(whitespace-line ((,class (:background ,zenburn-bg :foreground ,zenburn-magenta))))
    `(whitespace-space-before-tab ((,class (:background ,zenburn-orange :foreground ,zenburn-orange))))
    `(whitespace-indentation ((,class (:background ,zenburn-yellow :foreground ,zenburn-red))))
    `(whitespace-empty ((,class (:background ,zenburn-yellow))))
    `(whitespace-space-after-tab ((,class (:background ,zenburn-yellow :foreground ,zenburn-red))))
;;;;; yascroll
    `(yascroll:thumb-text-area ((,class (:background ,zenburn-bg-1))))
    `(yascroll:thumb-fringe ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-bg-1))))
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
