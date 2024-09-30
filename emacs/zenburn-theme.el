(deftheme zenburn "The Zenburn color theme.")

(defgroup zenburn-theme nil
  "Zenburn theme."
  :group 'faces
  :prefix "zenburn-"
  :link '(url-link :tag "GitHub" "http://github.com/bbatsov/zenburn-emacs")
  :tag "Zenburn theme")

(defcustom zenburn-override-colors-alist '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist."
  :group 'zenburn-theme
  :type '(alist
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
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

;;; Color Palette

(defvar zenburn-default-colors-alist
  '(("zenburn-fg-1"     . "#656555")
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
  `(let ((class '((class color) (min-colors 89)))
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
   `(fringe ((,class (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(header-line ((,class (:foreground ,zenburn-yellow
                                       :background ,zenburn-bg-1
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
                           :background ,zenburn-bg-1))))
   `(mode-line-buffer-id ((,class (:foreground ,zenburn-yellow :weight bold))))
   `(mode-line-inactive
     ((,class (:foreground ,zenburn-green-2
                           :background ,zenburn-bg-05))))
   `(region ((,class (:background ,zenburn-bg-1 :extend t))))
   `(secondary-selection ((,class (:background ,zenburn-bg+2))))
   `(trailing-whitespace ((,class (:background ,zenburn-red))))
   `(vertical-border ((,class (:foreground ,zenburn-fg-1))))
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
   `(line-number ((,class (:inherit default :foreground ,zenburn-bg+3 :background ,zenburn-bg))))
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

;;;;; ace-jump
   `(ace-jump-face-background
     ((,class (:foreground ,zenburn-fg-1 :background ,zenburn-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((,class (:foreground ,zenburn-green+2 :background ,zenburn-bg :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((,class (:foreground ,zenburn-fg-1 :background ,zenburn-bg :inverse-video nil))))
   `(aw-leading-char-face ((,class (:inherit aw-mode-line-face))))
;;;;; adoc-mode
   `(adoc-anchor-face ((,class (:foreground ,zenburn-blue+1))))
   `(adoc-code-face ((,class (:inherit font-lock-constant-face))))
   `(adoc-command-face ((,class (:foreground ,zenburn-yellow))))
   `(adoc-emphasis-face ((,class (:inherit bold))))
   `(adoc-internal-reference-face ((,class (:foreground ,zenburn-yellow-2 :underline t))))
   `(adoc-list-face ((,class (:foreground ,zenburn-fg+1))))
   `(adoc-meta-face ((,class (:foreground ,zenburn-yellow))))
   `(adoc-meta-hide-face ((,class (:foreground ,zenburn-yellow))))
   `(adoc-secondary-text-face ((,class (:foreground ,zenburn-yellow-1))))
   `(adoc-title-0-face ((,class (:inherit font-lock-function-name-face :weight bold))))
   `(adoc-title-1-face ((,class (:inherit font-lock-function-name-face :weight bold))))
   `(adoc-title-2-face ((,class (:inherit font-lock-function-name-face :weight bold))))
   `(adoc-title-3-face ((,class (:inherit font-lock-function-name-face :weight bold))))
   `(adoc-title-4-face ((,class (:inherit font-lock-function-name-face :weight bold))))
   `(adoc-typewriter-face ((,class (:inherit font-lock-constant-face))))
   `(adoc-verbatim-face ((,class (:inherit font-lock-constant-face))))
   `(adoc-value-face ((,class (:foreground ,zenburn-yellow))))
;;;;; android mode
   `(android-mode-debug-face ((,class (:foreground ,zenburn-green+1))))
   `(android-mode-error-face ((,class (:foreground ,zenburn-orange :weight bold))))
   `(android-mode-info-face ((,class (:foreground ,zenburn-fg))))
   `(android-mode-verbose-face ((,class (:foreground ,zenburn-green))))
   `(android-mode-warning-face ((,class (:foreground ,zenburn-yellow))))
;;;;; anzu
   `(anzu-mode-line ((,class (:foreground ,zenburn-cyan :weight bold))))
   `(anzu-mode-line-no-match ((,class (:foreground ,zenburn-red :weight bold))))
   `(anzu-match-1 ((,class (:foreground ,zenburn-bg :background ,zenburn-green))))
   `(anzu-match-2 ((,class (:foreground ,zenburn-bg :background ,zenburn-orange))))
   `(anzu-match-3 ((,class (:foreground ,zenburn-bg :background ,zenburn-blue))))
   `(anzu-replace-to ((,class (:inherit anzu-replace-highlight :foreground ,zenburn-yellow))))
;;;;; auctex
   `(font-latex-bold-face ((,class (:inherit bold))))
   `(font-latex-warning-face ((,class (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((,class (:foreground ,zenburn-red :weight bold ))))
   `(font-latex-sedate-face ((,class (:foreground ,zenburn-yellow))))
   `(font-latex-italic-face ((,class (:foreground ,zenburn-cyan :slant italic))))
   `(font-latex-string-face ((,class (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((,class (:foreground ,zenburn-orange))))
   `(font-latex-script-char-face ((,class (:foreground ,zenburn-orange))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((,class (:foreground ,zenburn-yellow :weight bold))))
   `(agda2-highlight-string-face ((,class (:foreground ,zenburn-red))))
   `(agda2-highlight-symbol-face ((,class (:foreground ,zenburn-orange))))
   `(agda2-highlight-primitive-type-face ((,class (:foreground ,zenburn-blue-1))))
   `(agda2-highlight-inductive-constructor-face ((,class (:foreground ,zenburn-fg))))
   `(agda2-highlight-coinductive-constructor-face ((,class (:foreground ,zenburn-fg))))
   `(agda2-highlight-datatype-face ((,class (:foreground ,zenburn-blue))))
   `(agda2-highlight-function-face ((,class (:foreground ,zenburn-blue))))
   `(agda2-highlight-module-face ((,class (:foreground ,zenburn-blue-1))))
   `(agda2-highlight-error-face ((,class (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(agda2-highlight-unsolved-meta-face ((,class (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(agda2-highlight-unsolved-constraint-face ((,class (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(agda2-highlight-termination-problem-face ((,class (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(agda2-highlight-incomplete-pattern-face ((,class (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(agda2-highlight-typechecks-face ((,class (:background ,zenburn-red-4))))
;;;;; auto-complete
   `(ac-candidate-face ((,class (:background ,zenburn-bg+3 :foreground ,zenburn-bg-2))))
   `(ac-selection-face ((,class (:background ,zenburn-blue-4 :foreground ,zenburn-fg))))
   `(popup-tip-face ((,class (:background ,zenburn-yellow-2 :foreground ,zenburn-bg-2))))
   `(popup-menu-mouse-face ((,class (:background ,zenburn-yellow-2 :foreground ,zenburn-bg-2))))
   `(popup-summary-face ((,class (:background ,zenburn-bg+3 :foreground ,zenburn-bg-2))))
   `(popup-scroll-bar-foreground-face ((,class (:background ,zenburn-blue-5))))
   `(popup-scroll-bar-background-face ((,class (:background ,zenburn-bg-1))))
   `(popup-isearch-match ((,class (:background ,zenburn-bg :foreground ,zenburn-fg))))
;;;;; avy
   `(avy-background-face
     ((,class (:foreground ,zenburn-fg-1 :background ,zenburn-bg :inverse-video nil))))
   `(avy-lead-face-0
     ((,class (:foreground ,zenburn-green+3 :background ,zenburn-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((,class (:foreground ,zenburn-yellow :background ,zenburn-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((,class (:foreground ,zenburn-red+1 :background ,zenburn-bg :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((,class (:foreground ,zenburn-cyan :background ,zenburn-bg :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((,class (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
   `(company-tooltip-annotation ((,class (:foreground ,zenburn-orange :background ,zenburn-bg+1))))
   `(company-tooltip-annotation-selection ((,class (:foreground ,zenburn-orange :background ,zenburn-bg-1))))
   `(company-tooltip-selection ((,class (:foreground ,zenburn-fg :background ,zenburn-bg-1))))
   `(company-tooltip-mouse ((,class (:background ,zenburn-bg-1))))
   `(company-tooltip-common ((,class (:foreground ,zenburn-green+2))))
   `(company-tooltip-common-selection ((,class (:foreground ,zenburn-green+2))))
   `(company-scrollbar-fg ((,class (:background ,zenburn-bg-1))))
   `(company-scrollbar-bg ((,class (:background ,zenburn-bg+2))))
   `(company-preview ((,class (:background ,zenburn-green+2))))
   `(company-preview-common ((,class (:foreground ,zenburn-green+2 :background ,zenburn-bg-1))))
;;;;; corfu
   `(corfu-default ((,class (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
   `(corfu-current ((,class (:foreground ,zenburn-fg :background ,zenburn-bg-1))))
   `(corfu-bar ((,class (:background ,zenburn-bg-1))))
   `(corfu-bar ((,class (:background ,zenburn-bg-2))))
;;;;; bm
   `(bm-face ((,class (:background ,zenburn-yellow-1 :foreground ,zenburn-bg))))
   `(bm-fringe-face ((,class (:background ,zenburn-yellow-1 :foreground ,zenburn-bg))))
   `(bm-fringe-persistent-face ((,class (:background ,zenburn-green-2 :foreground ,zenburn-bg))))
   `(bm-persistent-face ((,class (:background ,zenburn-green-2 :foreground ,zenburn-bg))))
;;;;; calfw
   `(cfw:face-annotation ((,class (:foreground ,zenburn-red :inherit cfw:face-day-title))))
   `(cfw:face-day-title ((,class nil)))
   `(cfw:face-default-content ((,class (:foreground ,zenburn-green))))
   `(cfw:face-default-day ((,class (:weight bold))))
   `(cfw:face-disable ((,class (:foreground ,zenburn-fg-1))))
   `(cfw:face-grid ((,class (:inherit shadow))))
   `(cfw:face-header ((,class (:inherit font-lock-keyword-face))))
   `(cfw:face-holiday ((,class (:inherit cfw:face-sunday))))
   `(cfw:face-periods ((,class (:foreground ,zenburn-cyan))))
   `(cfw:face-saturday ((,class (:foreground ,zenburn-blue :weight bold))))
   `(cfw:face-select ((,class (:background ,zenburn-blue-5))))
   `(cfw:face-sunday ((,class (:foreground ,zenburn-red :weight bold))))
   `(cfw:face-title ((,class (:height 2.0 :inherit (variable-pitch font-lock-keyword-face)))))
   `(cfw:face-today ((,class (:foreground ,zenburn-cyan :weight bold))))
   `(cfw:face-today-title ((,class (:inherit highlight bold))))
   `(cfw:face-toolbar ((,class (:background ,zenburn-blue-5))))
   `(cfw:face-toolbar-button-off ((,class (:underline nil :inherit link))))
   `(cfw:face-toolbar-button-on ((,class (:underline nil :inherit link-visited))))
;;;;; centaur-tabs
   `(centaur-tabs-default ((,class (:background ,zenburn-bg :foreground ,zenburn-fg :box nil))))
   `(centaur-tabs-selected ((,class (:background ,zenburn-bg :foreground ,zenburn-fg+2 :box nil))))
   `(centaur-tabs-unselected ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-fg-05 :box nil))))
   `(centaur-tabs-selected-modified ((,class (:background ,zenburn-bg :foreground ,zenburn-orange :box nil))))
   `(centaur-tabs-unselected-modified ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-orange :box nil))))
   `(centaur-tabs-active-bar-face ((,class (:background ,zenburn-yellow :box nil))))
   `(centaur-tabs-modified-marker-selected ((,class (:inherit 'centaur-tabs-selected-modified :foreground ,zenburn-yellow :box nil))))
   `(centaur-tabs-modified-marker-unselected ((,class (:inherit 'centaur-tabs-unselected-modified :foreground ,zenburn-yellow :box nil))))
;;;;; cider
   `(cider-result-overlay-face ((,class (:background unspecified))))
   `(cider-enlightened-face ((,class (:box (:color ,zenburn-orange :line-width -1)))))
   `(cider-enlightened-local-face ((,class (:weight bold :foreground ,zenburn-green+1))))
   `(cider-deprecated-face ((,class (:background ,zenburn-yellow-2))))
   `(cider-instrumented-face ((,class (:box (:color ,zenburn-red :line-width -1)))))
   `(cider-traced-face ((,class (:box (:color ,zenburn-cyan :line-width -1)))))
   `(cider-test-failure-face ((,class (:background ,zenburn-red-4))))
   `(cider-test-error-face ((,class (:background ,zenburn-magenta))))
   `(cider-test-success-face ((,class (:background ,zenburn-green-2))))
   `(cider-fringe-good-face ((,class (:foreground ,zenburn-green+4))))
;;;;; circe
   `(circe-highlight-nick-face ((,class (:foreground ,zenburn-cyan))))
   `(circe-my-message-face ((,class (:foreground ,zenburn-fg))))
   `(circe-fool-face ((,class (:foreground ,zenburn-red+1))))
   `(circe-topic-diff-removed-face ((,class (:foreground ,zenburn-red :weight bold))))
   `(circe-originator-face ((,class (:foreground ,zenburn-fg))))
   `(circe-server-face ((,class (:foreground ,zenburn-green))))
   `(circe-topic-diff-new-face ((,class (:foreground ,zenburn-orange :weight bold))))
   `(circe-prompt-face ((,class (:foreground ,zenburn-orange :background ,zenburn-bg :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((,class :foreground ,zenburn-fg)))
   `(context-coloring-level-1-face ((,class :foreground ,zenburn-cyan)))
   `(context-coloring-level-2-face ((,class :foreground ,zenburn-green+4)))
   `(context-coloring-level-3-face ((,class :foreground ,zenburn-yellow)))
   `(context-coloring-level-4-face ((,class :foreground ,zenburn-orange)))
   `(context-coloring-level-5-face ((,class :foreground ,zenburn-magenta)))
   `(context-coloring-level-6-face ((,class :foreground ,zenburn-blue+1)))
   `(context-coloring-level-7-face ((,class :foreground ,zenburn-green+2)))
   `(context-coloring-level-8-face ((,class :foreground ,zenburn-yellow-2)))
   `(context-coloring-level-9-face ((,class :foreground ,zenburn-red+1)))
;;;;; coq
   `(coq-solve-tactics-face ((,class (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((,class (:background ,zenburn-blue :foreground ,zenburn-bg))))
   `(ctbl:face-continue-bar ((,class (:background ,zenburn-bg-05 :foreground ,zenburn-bg))))
   `(ctbl:face-row-select ((,class (:background ,zenburn-cyan :foreground ,zenburn-bg))))
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
   `(diff-header ((,class (:background ,zenburn-bg+2))))
   `(diff-file-header ((,class (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
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
;;;;; doom-modeline
   `(doom-modeline-bar  ((,class (:background ,zenburn-yellow))))
   `(doom-modeline-inactive-bar  ((,class (:background nil))))
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
;;;;; egg
   `(egg-text-base ((,class (:foreground ,zenburn-fg))))
   `(egg-help-header-1 ((,class (:foreground ,zenburn-yellow))))
   `(egg-help-header-2 ((,class (:foreground ,zenburn-green+3))))
   `(egg-branch ((,class (:foreground ,zenburn-yellow))))
   `(egg-branch-mono ((,class (:foreground ,zenburn-yellow))))
   `(egg-term ((,class (:foreground ,zenburn-yellow))))
   `(egg-diff-add ((,class (:foreground ,zenburn-green+4))))
   `(egg-diff-del ((,class (:foreground ,zenburn-red+1))))
   `(egg-diff-file-header ((,class (:foreground ,zenburn-yellow-2))))
   `(egg-section-title ((,class (:foreground ,zenburn-yellow))))
   `(egg-stash-mono ((,class (:foreground ,zenburn-green+4))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((,class (:foreground ,zenburn-red))))
   `(elfeed-log-info-level-face ((,class (:foreground ,zenburn-blue))))
   `(elfeed-log-warn-level-face ((,class (:foreground ,zenburn-yellow))))
   `(elfeed-search-date-face ((,class (:foreground ,zenburn-yellow-1 :underline t
                                                   :weight bold))))
   `(elfeed-search-tag-face ((,class (:foreground ,zenburn-green))))
   `(elfeed-search-feed-face ((,class (:foreground ,zenburn-cyan))))
   `(elfeed-search-title-face ((,class (:foreground ,zenburn-fg-05))))
   `(elfeed-search-unread-title-face ((,class (:foreground ,zenburn-fg :weight bold))))
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
;;;;; erc
   `(erc-action-face ((,class (:inherit erc-default-face))))
   `(erc-bold-face ((,class (:weight bold))))
   `(erc-current-nick-face ((,class (:foreground ,zenburn-blue :weight bold))))
   `(erc-dangerous-host-face ((,class (:inherit font-lock-warning-face))))
   `(erc-default-face ((,class (:foreground ,zenburn-fg))))
   `(erc-direct-msg-face ((,class (:inherit erc-default-face))))
   `(erc-error-face ((,class (:inherit font-lock-warning-face))))
   `(erc-fool-face ((,class (:inherit erc-default-face))))
   `(erc-highlight-face ((,class (:inherit hover-highlight))))
   `(erc-input-face ((,class (:foreground ,zenburn-yellow))))
   `(erc-keyword-face ((,class (:foreground ,zenburn-blue :weight bold))))
   `(erc-nick-default-face ((,class (:foreground ,zenburn-yellow :weight bold))))
   `(erc-my-nick-face ((,class (:foreground ,zenburn-red :weight bold))))
   `(erc-nick-msg-face ((,class (:inherit erc-default-face))))
   `(erc-notice-face ((,class (:foreground ,zenburn-green))))
   `(erc-pal-face ((,class (:foreground ,zenburn-orange :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,zenburn-orange :background ,zenburn-bg :weight bold))))
   `(erc-timestamp-face ((,class (:foreground ,zenburn-green+4))))
   `(erc-underline-face ((,class (:underline t))))
;;;;; eros
   `(eros-result-overlay-face ((,class (:background unspecified))))
;;;;; ert
   `(ert-test-result-expected ((,class (:foreground ,zenburn-green+4 :background ,zenburn-bg))))
   `(ert-test-result-unexpected ((,class (:foreground ,zenburn-red :background ,zenburn-bg))))
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
;;;;; flx
   `(flx-highlight-face ((,class (:foreground ,zenburn-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red-1) :inherit unspecified))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-yellow) :inherit unspecified))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-cyan) :inherit unspecified))))
   `(flycheck-fringe-error ((,class (:foreground ,zenburn-red-1 :weight bold))))
   `(flycheck-fringe-warning ((,class (:foreground ,zenburn-yellow :weight bold))))
   `(flycheck-fringe-info ((,class (:foreground ,zenburn-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red)
                   :inherit unspecified :foreground unspecified :background unspecified))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-green)
                   :inherit unspecified :foreground unspecified :background unspecified))))
   `(flymake-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red)
                   :inherit unspecified :foreground unspecified :background unspecified))))
   `(flymake-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))))
   `(flymake-note
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-green)
                   :inherit unspecified :foreground unspecified :background unspecified))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange) :inherit unspecified))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red) :inherit unspecified))))
;;;;; full-ack
   `(ack-separator ((,class (:foreground ,zenburn-fg))))
   `(ack-file ((,class (:foreground ,zenburn-blue))))
   `(ack-line ((,class (:foreground ,zenburn-yellow))))
   `(ack-match ((,class (:foreground ,zenburn-orange :background ,zenburn-bg-1 :weight bold))))
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
;;;;; go-guru
   `(go-guru-hl-identifier-face ((,class (:foreground ,zenburn-bg-1 :background ,zenburn-green+1))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((,class (:foreground ,zenburn-blue))))
   `(guide-key/key-face ((,class (:foreground ,zenburn-green))))
   `(guide-key/prefix-command-face ((,class (:foreground ,zenburn-green+1))))
;;;;; hackernews
   `(hackernews-comment-count ((,class (:inherit link-visited :underline nil))))
   `(hackernews-link          ((,class (:inherit link         :underline nil))))
;;;;; helm
   `(helm-header
     ((,class (:foreground ,zenburn-green
                           :background ,zenburn-bg
                           :underline nil
                           :box nil
                           :extend t))))
   `(helm-source-header
     ((,class (:foreground ,zenburn-yellow
                           :background ,zenburn-bg-1
                           :underline nil
                           :weight bold
                           :extend t))))
   `(helm-selection ((,class (:background ,zenburn-bg+1 :underline nil))))
   `(helm-selection-line ((,class (:background ,zenburn-bg+1))))
   `(helm-visible-mark ((,class (:foreground ,zenburn-bg :background ,zenburn-yellow-2))))
   `(helm-candidate-number ((,class (:foreground ,zenburn-green+4 :background ,zenburn-bg-1))))
   `(helm-separator ((,class (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(helm-time-zone-current ((,class (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
   `(helm-time-zone-home ((,class (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(helm-bookmark-addressbook ((,class (:foreground ,zenburn-orange :background ,zenburn-bg))))
   `(helm-bookmark-directory ((,class (:foreground unspecified :background unspecified :inherit helm-ff-directory))))
   `(helm-bookmark-file ((,class (:foreground unspecified :background unspecified :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((,class (:foreground ,zenburn-magenta :background ,zenburn-bg))))
   `(helm-bookmark-info ((,class (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
   `(helm-bookmark-man ((,class (:foreground ,zenburn-yellow :background ,zenburn-bg))))
   `(helm-bookmark-w3m ((,class (:foreground ,zenburn-magenta :background ,zenburn-bg))))
   `(helm-buffer-not-saved ((,class (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(helm-buffer-process ((,class (:foreground ,zenburn-cyan :background ,zenburn-bg))))
   `(helm-buffer-saved-out ((,class (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(helm-buffer-size ((,class (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))
   `(helm-ff-directory ((,class (:foreground ,zenburn-cyan :background ,zenburn-bg :weight bold))))
   `(helm-ff-file ((,class (:foreground ,zenburn-fg :background ,zenburn-bg :weight normal))))
   `(helm-ff-file-extension ((,class (:foreground ,zenburn-fg :background ,zenburn-bg :weight normal))))
   `(helm-ff-executable ((,class (:foreground ,zenburn-green+2 :background ,zenburn-bg :weight normal))))
   `(helm-ff-invalid-symlink ((,class (:foreground ,zenburn-red :background ,zenburn-bg :weight bold))))
   `(helm-ff-symlink ((,class (:foreground ,zenburn-yellow :background ,zenburn-bg :weight bold))))
   `(helm-ff-prefix ((,class (:foreground ,zenburn-bg :background ,zenburn-yellow :weight normal))))
   `(helm-grep-cmd-line ((,class (:foreground ,zenburn-cyan :background ,zenburn-bg))))
   `(helm-grep-file ((,class (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(helm-grep-finish ((,class (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
   `(helm-grep-lineno ((,class (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))
   `(helm-grep-match ((,class (:foreground unspecified :background unspecified :inherit helm-match))))
   `(helm-grep-running ((,class (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(helm-match ((,class (:foreground ,zenburn-orange :background ,zenburn-bg-1 :weight bold))))
   `(helm-moccur-buffer ((,class (:foreground ,zenburn-cyan :background ,zenburn-bg))))
   `(helm-mu-contacts-address-face ((,class (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))
   `(helm-mu-contacts-name-face ((,class (:foreground ,zenburn-fg :background ,zenburn-bg))))
;;;;; helm-lxc
   `(helm-lxc-face-frozen ((,class (:foreground ,zenburn-blue :background ,zenburn-bg))))
   `(helm-lxc-face-running ((,class (:foreground ,zenburn-green :background ,zenburn-bg))))
   `(helm-lxc-face-stopped ((,class (:foreground ,zenburn-red :background ,zenburn-bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((,class (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
   `(helm-swoop-target-word-face ((,class (:foreground ,zenburn-yellow :background ,zenburn-bg+2 :weight bold))))
;;;;; highlight-numbers
   `(highlight-numbers-number ((,class (:foreground ,zenburn-blue))))
;;;;; highlight-symbol
   `(highlight-symbol-face ((,class (:background ,zenburn-bg+2))))
;;;;; highlight-thing
   `(highlight-thing ((,class (:background ,zenburn-bg+2))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,zenburn-bg-05))))
   `(hl-line ((,class (:background ,zenburn-bg-05 :extend t))))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,zenburn-bg+1))))
;;;;; hydra
   `(hydra-face-red ((,class (:foreground ,zenburn-red-1 :background ,zenburn-bg))))
   `(hydra-face-amaranth ((,class (:foreground ,zenburn-red-3 :background ,zenburn-bg))))
   `(hydra-face-blue ((,class (:foreground ,zenburn-blue :background ,zenburn-bg))))
   `(hydra-face-pink ((,class (:foreground ,zenburn-magenta :background ,zenburn-bg))))
   `(hydra-face-teal ((,class (:foreground ,zenburn-cyan :background ,zenburn-bg))))
;;;;; info+
   `(info-command-ref-item ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))
   `(info-constant-ref-item ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-magenta))))
   `(info-double-quoted-name ((,class (:inherit font-lock-comment-face))))
   `(info-file ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-yellow))))
   `(info-function-ref-item ((,class (:background ,zenburn-bg-1 :inherit font-lock-function-name-face))))
   `(info-macro-ref-item ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-yellow))))
   `(info-menu ((,class (:foreground ,zenburn-yellow))))
   `(info-quoted-name ((,class (:inherit font-lock-constant-face))))
   `(info-reference-item ((,class (:background ,zenburn-bg-1))))
   `(info-single-quote ((,class (:inherit font-lock-keyword-face))))
   `(info-special-form-ref-item ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-yellow))))
   `(info-string ((,class (:inherit font-lock-string-face))))
   `(info-syntax-class-item ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-blue+1))))
   `(info-user-option-ref-item ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-red))))
   `(info-variable-ref-item ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))
;;;;; irfc
   `(irfc-head-name-face ((,class (:foreground ,zenburn-red :weight bold))))
   `(irfc-head-number-face ((,class (:foreground ,zenburn-red :weight bold))))
   `(irfc-reference-face ((,class (:foreground ,zenburn-blue-1 :weight bold))))
   `(irfc-requirement-keyword-face ((,class (:inherit font-lock-keyword-face))))
   `(irfc-rfc-link-face ((,class (:inherit link))))
   `(irfc-rfc-number-face ((,class (:foreground ,zenburn-cyan :weight bold))))
   `(irfc-std-number-face ((,class (:foreground ,zenburn-green+4 :weight bold))))
   `(irfc-table-item-face ((,class (:foreground ,zenburn-green+3))))
   `(irfc-title-face ((,class (:foreground ,zenburn-yellow
                                           :underline t :weight bold))))
;;;;; ivy
   `(ivy-confirm-face ((,class (:foreground ,zenburn-green :background ,zenburn-bg))))
   `(ivy-current-match ((,class (:foreground ,zenburn-yellow :weight bold :underline t))))
   `(ivy-cursor ((,class (:foreground ,zenburn-bg :background ,zenburn-fg))))
   `(ivy-match-required-face ((,class (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(ivy-minibuffer-match-face-1 ((,class (:background ,zenburn-bg+1))))
   `(ivy-minibuffer-match-face-2 ((,class (:background ,zenburn-green-2))))
   `(ivy-minibuffer-match-face-3 ((,class (:background ,zenburn-green))))
   `(ivy-minibuffer-match-face-4 ((,class (:background ,zenburn-green+1))))
   `(ivy-remote ((,class (:foreground ,zenburn-blue :background ,zenburn-bg))))
   `(ivy-subdir ((,class (:foreground ,zenburn-yellow :background ,zenburn-bg))))
;;;;; ido-mode
   `(ido-first-match ((,class (:foreground ,zenburn-yellow :weight bold))))
   `(ido-only-match ((,class (:foreground ,zenburn-orange :weight bold))))
   `(ido-subdir ((,class (:foreground ,zenburn-yellow))))
   `(ido-indicator ((,class (:foreground ,zenburn-yellow :background ,zenburn-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((,class (:background ,zenburn-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((,class (:foreground ,zenburn-green+2))))
   `(jabber-roster-user-online ((,class (:foreground ,zenburn-blue-1))))
   `(jabber-roster-user-dnd ((,class (:foreground ,zenburn-red+1))))
   `(jabber-roster-user-xa ((,class (:foreground ,zenburn-magenta))))
   `(jabber-roster-user-chatty ((,class (:foreground ,zenburn-orange))))
   `(jabber-roster-user-error ((,class (:foreground ,zenburn-red+1))))
   `(jabber-rare-time-face ((,class (:foreground ,zenburn-green+1))))
   `(jabber-chat-prompt-local ((,class (:foreground ,zenburn-blue-1))))
   `(jabber-chat-prompt-foreign ((,class (:foreground ,zenburn-red+1))))
   `(jabber-chat-prompt-system ((,class (:foreground ,zenburn-green+3))))
   `(jabber-activity-face((,class (:foreground ,zenburn-red+1))))
   `(jabber-activity-personal-face ((,class (:foreground ,zenburn-blue+1))))
   `(jabber-title-small ((,class (:height 1.1 :weight bold))))
   `(jabber-title-medium ((,class (:height 1.2 :weight bold))))
   `(jabber-title-large ((,class (:height 1.3 :weight bold))))
;;;;; js2-mode
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
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((,class (:foreground ,zenburn-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((,class (:foreground ,zenburn-fg :weight normal))))
   `(ledger-font-payee-pending-face ((,class (:foreground ,zenburn-red :weight normal))))
   `(ledger-font-xact-highlight-face ((,class (:background ,zenburn-bg+1))))
   `(ledger-font-auto-xact-face ((,class (:foreground ,zenburn-yellow-1 :weight normal))))
   `(ledger-font-periodic-xact-face ((,class (:foreground ,zenburn-green :weight normal))))
   `(ledger-font-pending-face ((,class (:foreground ,zenburn-orange weight: normal))))
   `(ledger-font-other-face ((,class (:foreground ,zenburn-fg))))
   `(ledger-font-posting-date-face ((,class (:foreground ,zenburn-orange :weight normal))))
   `(ledger-font-posting-account-face ((,class (:foreground ,zenburn-blue-1))))
   `(ledger-font-posting-account-cleared-face ((,class (:foreground ,zenburn-fg))))
   `(ledger-font-posting-account-pending-face ((,class (:foreground ,zenburn-orange))))
   `(ledger-font-posting-amount-face ((,class (:foreground ,zenburn-orange))))
   `(ledger-occur-narrowed-face ((,class (:foreground ,zenburn-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((,class (:background ,zenburn-bg+1))))
   `(ledger-font-comment-face ((,class (:foreground ,zenburn-green))))
   `(ledger-font-reconciler-uncleared-face ((,class (:foreground ,zenburn-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((,class (:foreground ,zenburn-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((,class (:foreground ,zenburn-orange :weight normal))))
   `(ledger-font-report-clickable-face ((,class (:foreground ,zenburn-orange :weight normal))))
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
   `(ruler-mode-current-column ((,class (:foreground ,zenburn-yellow))))
   `(ruler-mode-default ((,class (:foreground ,zenburn-green+2 :background ,zenburn-bg))))

;;;;; lui
   `(lui-time-stamp-face ((,class (:foreground ,zenburn-blue-1))))
   `(lui-hilight-face ((,class (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
   `(lui-button-face ((,class (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((,class (:foreground ,zenburn-green+2 :background ,zenburn-bg-1))))
   `(macrostep-gensym-2
     ((,class (:foreground ,zenburn-red+1 :background ,zenburn-bg-1))))
   `(macrostep-gensym-3
     ((,class (:foreground ,zenburn-blue+1 :background ,zenburn-bg-1))))
   `(macrostep-gensym-4
     ((,class (:foreground ,zenburn-magenta :background ,zenburn-bg-1))))
   `(macrostep-gensym-5
     ((,class (:foreground ,zenburn-yellow :background ,zenburn-bg-1))))
   `(macrostep-expansion-highlight-face
     ((,class (:inherit highlight))))
   `(macrostep-macro-face
     ((,class (:underline t))))
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
   `(magit-branch-current ((,class (:foreground ,zenburn-blue   :weight bold))))
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
;;;;; mew
   `(mew-face-header-subject ((,class (:foreground ,zenburn-orange))))
   `(mew-face-header-from ((,class (:foreground ,zenburn-yellow))))
   `(mew-face-header-date ((,class (:foreground ,zenburn-green))))
   `(mew-face-header-to ((,class (:foreground ,zenburn-red))))
   `(mew-face-header-key ((,class (:foreground ,zenburn-green))))
   `(mew-face-header-private ((,class (:foreground ,zenburn-green))))
   `(mew-face-header-important ((,class (:foreground ,zenburn-blue))))
   `(mew-face-header-marginal ((,class (:foreground ,zenburn-fg :weight bold))))
   `(mew-face-header-warning ((,class (:foreground ,zenburn-red))))
   `(mew-face-header-xmew ((,class (:foreground ,zenburn-green))))
   `(mew-face-header-xmew-bad ((,class (:foreground ,zenburn-red))))
   `(mew-face-body-url ((,class (:foreground ,zenburn-orange))))
   `(mew-face-body-comment ((,class (:foreground ,zenburn-fg :slant italic))))
   `(mew-face-body-cite1 ((,class (:foreground ,zenburn-green))))
   `(mew-face-body-cite2 ((,class (:foreground ,zenburn-blue))))
   `(mew-face-body-cite3 ((,class (:foreground ,zenburn-orange))))
   `(mew-face-body-cite4 ((,class (:foreground ,zenburn-yellow))))
   `(mew-face-body-cite5 ((,class (:foreground ,zenburn-red))))
   `(mew-face-mark-review ((,class (:foreground ,zenburn-blue))))
   `(mew-face-mark-escape ((,class (:foreground ,zenburn-green))))
   `(mew-face-mark-delete ((,class (:foreground ,zenburn-red))))
   `(mew-face-mark-unlink ((,class (:foreground ,zenburn-yellow))))
   `(mew-face-mark-refile ((,class (:foreground ,zenburn-green))))
   `(mew-face-mark-unread ((,class (:foreground ,zenburn-red-2))))
   `(mew-face-eof-message ((,class (:foreground ,zenburn-green))))
   `(mew-face-eof-part ((,class (:foreground ,zenburn-yellow))))
;;;;; mic-paren
   `(paren-face-match ((,class (:foreground ,zenburn-cyan :background ,zenburn-bg :weight bold))))
   `(paren-face-mismatch ((,class (:foreground ,zenburn-bg :background ,zenburn-magenta :weight bold))))
   `(paren-face-no-match ((,class (:foreground ,zenburn-bg :background ,zenburn-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((,class (:foreground ,zenburn-blue))))
   `(mingus-pausing-face ((,class (:foreground ,zenburn-magenta))))
   `(mingus-playing-face ((,class (:foreground ,zenburn-cyan))))
   `(mingus-playlist-face ((,class (:foreground ,zenburn-cyan ))))
   `(mingus-mark-face ((,class (:bold t :foreground ,zenburn-magenta))))
   `(mingus-song-file-face ((,class (:foreground ,zenburn-yellow))))
   `(mingus-artist-face ((,class (:foreground ,zenburn-cyan))))
   `(mingus-album-face ((,class (:underline t :foreground ,zenburn-red+1))))
   `(mingus-album-stale-face ((,class (:foreground ,zenburn-red+1))))
   `(mingus-stopped-face ((,class (:foreground ,zenburn-red))))
;;;;; nav
   `(nav-face-heading ((,class (:foreground ,zenburn-yellow))))
   `(nav-face-button-num ((,class (:foreground ,zenburn-cyan))))
   `(nav-face-dir ((,class (:foreground ,zenburn-green))))
   `(nav-face-hdir ((,class (:foreground ,zenburn-red))))
   `(nav-face-file ((,class (:foreground ,zenburn-fg))))
   `(nav-face-hfile ((,class (:foreground ,zenburn-red-4))))
;;;;; merlin
   `(merlin-type-face ((,class (:inherit highlight))))
   `(merlin-compilation-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange)))))
   `(merlin-compilation-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red)))))
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
;;;;; mumamo
   `(mumamo-background-chunk-major ((,class (:background nil))))
   `(mumamo-background-chunk-submode1 ((,class (:background ,zenburn-bg-1))))
   `(mumamo-background-chunk-submode2 ((,class (:background ,zenburn-bg+2))))
   `(mumamo-background-chunk-submode3 ((,class (:background ,zenburn-bg+3))))
   `(mumamo-background-chunk-submode4 ((,class (:background ,zenburn-bg+1))))
;;;;; neotree
   `(neo-banner-face ((,class (:foreground ,zenburn-blue+1 :weight bold))))
   `(neo-header-face ((,class (:foreground ,zenburn-fg))))
   `(neo-root-dir-face ((,class (:foreground ,zenburn-blue+1 :weight bold))))
   `(neo-dir-link-face ((,class (:foreground ,zenburn-blue))))
   `(neo-file-link-face ((,class (:foreground ,zenburn-fg))))
   `(neo-expand-btn-face ((,class (:foreground ,zenburn-blue))))
   `(neo-vc-default-face ((,class (:foreground ,zenburn-fg+1))))
   `(neo-vc-user-face ((,class (:foreground ,zenburn-red :slant italic))))
   `(neo-vc-up-to-date-face ((,class (:foreground ,zenburn-fg))))
   `(neo-vc-edited-face ((,class (:foreground ,zenburn-magenta))))
   `(neo-vc-needs-merge-face ((,class (:foreground ,zenburn-red+1))))
   `(neo-vc-unlocked-changes-face ((,class (:foreground ,zenburn-red :background ,zenburn-blue-5))))
   `(neo-vc-added-face ((,class (:foreground ,zenburn-green+1))))
   `(neo-vc-conflict-face ((,class (:foreground ,zenburn-red+1))))
   `(neo-vc-missing-face ((,class (:foreground ,zenburn-red+1))))
   `(neo-vc-ignored-face ((,class (:foreground ,zenburn-fg-1))))
;;;;; notmuch
   `(notmuch-crypto-decryption ((,class (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(notmuch-crypto-part-header ((,class (:foreground ,zenburn-blue+1))))
   `(notmuch-crypto-signature-bad ((,class (:foreground ,zenburn-bg :background ,zenburn-red))))
   `(notmuch-crypto-signature-good ((,class (:foreground ,zenburn-bg :background ,zenburn-green+1))))
   `(notmuch-crypto-signature-good-key ((,class (:foreground ,zenburn-bg :background ,zenburn-orange))))
   `(notmuch-crypto-signature-unknown ((,class (:foreground ,zenburn-bg :background ,zenburn-red))))
   `(notmuch-hello-logo-background ((,class (:background ,zenburn-bg+2))))
   `(notmuch-message-summary-face ((,class (:background ,zenburn-bg-08))))
   `(notmuch-search-flagged-face ((,class (:foreground ,zenburn-blue+1))))
   `(notmuch-search-non-matching-authors ((,class (:foreground ,zenburn-fg-1))))
   `(notmuch-tag-added ((,class (:underline ,zenburn-green+1))))
   `(notmuch-tag-deleted ((,class (:strike-through ,zenburn-red))))
   `(notmuch-tag-face ((,class (:foreground ,zenburn-green+1))))
   `(notmuch-tag-flagged ((,class (:foreground ,zenburn-blue+1))))
   `(notmuch-tag-unread ((,class (:foreground ,zenburn-red))))
   `(notmuch-tree-match-author-face ((,class (:foreground ,zenburn-green+1))))
   `(notmuch-tree-match-tag-face ((,class (:foreground ,zenburn-green+1))))
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
   `(org-checkbox ((,class (:background ,zenburn-bg+2 :foreground ,zenburn-fg+1))))
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
;;;;; p4
   `(p4-depot-added-face ((,class :inherit diff-added)))
   `(p4-depot-branch-op-face ((,class :inherit diff-changed)))
   `(p4-depot-deleted-face ((,class :inherit diff-removed)))
   `(p4-depot-unmapped-face ((,class :inherit diff-changed)))
   `(p4-diff-change-face ((,class :inherit diff-changed)))
   `(p4-diff-del-face ((,class :inherit diff-removed)))
   `(p4-diff-file-face ((,class :inherit diff-file-header)))
   `(p4-diff-head-face ((,class :inherit diff-header)))
   `(p4-diff-ins-face ((,class :inherit diff-added)))
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
;;;;; proofgeneral
   `(proof-active-area-face ((,class (:underline t))))
   `(proof-boring-face ((,class (:foreground ,zenburn-fg :background ,zenburn-bg+2))))
   `(proof-command-mouse-highlight-face ((,class (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((,class (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((,class (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((,class (:foreground ,zenburn-bg :background ,zenburn-orange))))
   `(proof-error-face ((,class (:foreground ,zenburn-fg :background ,zenburn-red-4))))
   `(proof-highlight-dependency-face ((,class (:foreground ,zenburn-bg :background ,zenburn-yellow-1))))
   `(proof-highlight-dependent-face ((,class (:foreground ,zenburn-bg :background ,zenburn-orange))))
   `(proof-locked-face ((,class (:background ,zenburn-blue-5))))
   `(proof-mouse-highlight-face ((,class (:foreground ,zenburn-bg :background ,zenburn-orange))))
   `(proof-queue-face ((,class (:background ,zenburn-red-4))))
   `(proof-region-mouse-highlight-face ((,class (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((,class (:background ,zenburn-red-2))))
   `(proof-tacticals-name-face ((,class (:inherit font-lock-constant-face :foreground nil :background ,zenburn-bg))))
   `(proof-tactics-name-face ((,class (:inherit font-lock-constant-face :foreground nil :background ,zenburn-bg))))
   `(proof-warning-face ((,class (:foreground ,zenburn-bg :background ,zenburn-yellow-1))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((,class (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((,class (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,zenburn-fg))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,zenburn-green+4))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,zenburn-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,zenburn-cyan))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,zenburn-green+2))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,zenburn-blue+1))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,zenburn-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,zenburn-green+1))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,zenburn-blue-2))))
   `(rainbow-delimiters-depth-10-face ((,class (:foreground ,zenburn-orange))))
   `(rainbow-delimiters-depth-11-face ((,class (:foreground ,zenburn-green))))
   `(rainbow-delimiters-depth-12-face ((,class (:foreground ,zenburn-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((,class (:foreground ,zenburn-blue))))
   `(rcirc-other-nick ((,class (:foreground ,zenburn-orange))))
   `(rcirc-bright-nick ((,class (:foreground ,zenburn-blue+1))))
   `(rcirc-dim-nick ((,class (:foreground ,zenburn-blue-2))))
   `(rcirc-server ((,class (:foreground ,zenburn-green))))
   `(rcirc-server-prefix ((,class (:foreground ,zenburn-green+1))))
   `(rcirc-timestamp ((,class (:foreground ,zenburn-green+2))))
   `(rcirc-nick-in-message ((,class (:foreground ,zenburn-yellow))))
   `(rcirc-nick-in-message-full-line ((,class (:weight bold))))
   `(rcirc-prompt ((,class (:foreground ,zenburn-yellow :weight bold))))
   `(rcirc-track-nick ((,class (:inverse-video t))))
   `(rcirc-track-keyword ((,class (:weight bold))))
   `(rcirc-url ((,class (:weight bold))))
   `(rcirc-keyword ((,class (:foreground ,zenburn-yellow :weight bold))))
;;;;; re-builder
   `(reb-match-0 ((,class (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(reb-match-1 ((,class (:foreground ,zenburn-bg :background ,zenburn-blue))))
   `(reb-match-2 ((,class (:foreground ,zenburn-bg :background ,zenburn-orange))))
   `(reb-match-3 ((,class (:foreground ,zenburn-bg :background ,zenburn-red))))
;;;;; realgud
   `(realgud-overlay-arrow1 ((,class (:foreground ,zenburn-green))))
   `(realgud-overlay-arrow2 ((,class (:foreground ,zenburn-yellow))))
   `(realgud-overlay-arrow3 ((,class (:foreground ,zenburn-orange))))
   `(realgud-bp-enabled-face ((,class (:inherit error))))
   `(realgud-bp-disabled-face ((,class (:inherit secondary-selection))))
   `(realgud-bp-line-enabled-face ((,class (:box (:color ,zenburn-red :style nil)))))
   `(realgud-bp-line-disabled-face ((,class (:box (:color "grey70" :style nil)))))
   `(realgud-line-number ((,class (:foreground ,zenburn-yellow))))
   `(realgud-backtrace-number ((,class (:foreground ,zenburn-yellow, :weight bold))))
;;;;; regex-tool
   `(regex-tool-matched-face ((,class (:background ,zenburn-blue-4 :weight bold))))
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
;;;;; SLIME
   `(slime-repl-output-face ((,class (:foreground ,zenburn-red))))
   `(slime-repl-inputed-output-face ((,class (:foreground ,zenburn-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red)))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange)))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-yellow)))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-green)))))
   `(slime-highlight-face ((,class (:inherit highlight))))
;;;;; SLY
   `(sly-mrepl-output-face ((,class (:foreground ,zenburn-red))))
   `(sly-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red)))))
   `(sly-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange)))))
   `(sly-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-yellow)))))
   `(sly-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-green)))))
   `(sly-stickers-placed-face ((,class (:foreground ,zenburn-fg :background ,zenburn-bg+3))))
;;;;; solaire
   `(solaire-default-face ((,class (:inherit default :background ,zenburn-bg-08))))
   `(solaire-minibuffer-face ((,class (:inherit default :background ,zenburn-bg-08))))
   `(solaire-hl-line-face ((,class (:inherit hl-line :background ,zenburn-bg))))
   `(solaire-org-hide-face ((,class (:inherit org-hide :background ,zenburn-bg-08))))
;;;;; speedbar
   `(speedbar-button-face ((,class (:foreground ,zenburn-green+2))))
   `(speedbar-directory-face ((,class (:foreground ,zenburn-cyan))))
   `(speedbar-file-face ((,class (:foreground ,zenburn-fg))))
   `(speedbar-highlight-face ((,class (:foreground ,zenburn-bg :background ,zenburn-green+2))))
   `(speedbar-selected-face ((,class (:foreground ,zenburn-red))))
   `(speedbar-separator-face ((,class (:foreground ,zenburn-bg :background ,zenburn-blue-1))))
   `(speedbar-tag-face ((,class (:foreground ,zenburn-yellow))))
;;;;; swiper
   `(swiper-line-face ((,class (:underline t))))
;;;;; sx
   `(sx-custom-button
     ((,class (:background ,zenburn-fg :foreground ,zenburn-bg-1))))
   `(sx-question-list-answers
     ((,class (:foreground ,zenburn-green+3
                           :height 1.0 :inherit sx-question-list-parent))))
   `(sx-question-mode-accepted
     ((,class (:foreground ,zenburn-green+3
                           :height 1.3 :inherit sx-question-mode-title))))
   `(sx-question-mode-content-face ((,class (:inherit highlight))))
   `(sx-question-mode-kbd-tag
     ((,class (:box (:color ,zenburn-bg-1 :line-width 3 :style released-button)
                    :height 0.9 :weight semi-bold))))
;;;;; tabbar
   `(tabbar-button ((,class (:foreground ,zenburn-fg
                                         :background ,zenburn-bg))))
   `(tabbar-selected ((,class (:foreground ,zenburn-fg
                                           :background ,zenburn-bg))))
   `(tabbar-unselected ((,class (:foreground ,zenburn-fg
                                             :background ,zenburn-bg+1))))
;;;;; tab-bar
   `(tab-bar ((,class (:background ,zenburn-bg+1))))
   `(tab-bar-tab ((,class (:foreground ,zenburn-fg
                                       :background ,zenburn-bg
                                       :weight bold))))
   `(tab-bar-tab-inactive ((,class (:foreground ,zenburn-fg
                                                :background ,zenburn-bg+1))))
;;;;; tab-line
   `(tab-line ((,class (:background ,zenburn-bg+1))))
   `(tab-line-tab ((,class (:foreground ,zenburn-fg
                                        :background ,zenburn-bg
                                        :weight bold))))
   `(tab-line-tab-inactive ((,class (:foreground ,zenburn-fg
                                                 :background ,zenburn-bg+1))))
   `(tab-line-tab-current ((,class (:foreground ,zenburn-fg
                                                :background ,zenburn-bg+1))))
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
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((,class (:foreground ,zenburn-red-2))))
   `(wl-highlight-folder-many-face ((,class (:foreground ,zenburn-red-1))))
   `(wl-highlight-folder-path-face ((,class (:foreground ,zenburn-orange))))
   `(wl-highlight-folder-unread-face ((,class (:foreground ,zenburn-blue))))
   `(wl-highlight-folder-zero-face ((,class (:foreground ,zenburn-fg))))
   `(wl-highlight-folder-unknown-face ((,class (:foreground ,zenburn-blue))))
   `(wl-highlight-message-citation-header ((,class (:foreground ,zenburn-red-1))))
   `(wl-highlight-message-cited-text-1 ((,class (:foreground ,zenburn-red))))
   `(wl-highlight-message-cited-text-2 ((,class (:foreground ,zenburn-green+2))))
   `(wl-highlight-message-cited-text-3 ((,class (:foreground ,zenburn-blue))))
   `(wl-highlight-message-cited-text-4 ((,class (:foreground ,zenburn-blue+1))))
   `(wl-highlight-message-header-contents-face ((,class (:foreground ,zenburn-green))))
   `(wl-highlight-message-headers-face ((,class (:foreground ,zenburn-red+1))))
   `(wl-highlight-message-important-header-contents ((,class (:foreground ,zenburn-green+2))))
   `(wl-highlight-message-header-contents ((,class (:foreground ,zenburn-green+1))))
   `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,zenburn-green+2))))
   `(wl-highlight-message-signature ((,class (:foreground ,zenburn-green))))
   `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,zenburn-fg))))
   `(wl-highlight-summary-answered-face ((,class (:foreground ,zenburn-blue))))
   `(wl-highlight-summary-disposed-face ((,class (:foreground ,zenburn-fg
                                                              :slant italic))))
   `(wl-highlight-summary-new-face ((,class (:foreground ,zenburn-blue))))
   `(wl-highlight-summary-normal-face ((,class (:foreground ,zenburn-fg))))
   `(wl-highlight-summary-thread-top-face ((,class (:foreground ,zenburn-yellow))))
   `(wl-highlight-thread-indent-face ((,class (:foreground ,zenburn-magenta))))
   `(wl-highlight-summary-refiled-face ((,class (:foreground ,zenburn-fg))))
   `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((,class (:foreground ,zenburn-green+4))))
;;;;; window-tool-bar-mode
   `(window-tool-bar-button ((,class (:foreground ,zenburn-fg
                                                  :background ,zenburn-bg))))
   `(window-tool-bar-button-hover ((,class (:foreground ,zenburn-fg
                                                        :background ,zenburn-bg+1))))
   `(window-tool-bar-button-disabled ((,class (:foreground ,zenburn-fg
                                                           :background ,zenburn-bg+3))))
;;;;; xcscope
   `(cscope-file-face ((,class (:foreground ,zenburn-yellow :weight bold))))
   `(cscope-function-face ((,class (:foreground ,zenburn-cyan :weight bold))))
   `(cscope-line-number-face ((,class (:foreground ,zenburn-red :weight bold))))
   `(cscope-mouse-face ((,class (:foreground ,zenburn-bg :background ,zenburn-blue+1))))
   `(cscope-separator-face ((,class (:foreground ,zenburn-red :weight bold
                                                 :underline t :overline t))))
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
;;;;; company-quickhelp
   `(company-quickhelp-color-background ,zenburn-bg+1)
   `(company-quickhelp-color-foreground ,zenburn-fg)
;;;;; fill-column-indicator
   `(fci-rule-color ,zenburn-bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,zenburn-red ,zenburn-orange ,zenburn-yellow ,zenburn-green ,zenburn-green+4
                    ,zenburn-cyan ,zenburn-blue+1 ,zenburn-magenta))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,zenburn-fg . ,zenburn-bg-05))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,zenburn-red-1)
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

(provide-theme 'zenburn)
