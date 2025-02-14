(defmacro expand-cache-file (file)
  "Expand FILE from cache directory."
  `(expand-file-name ,file "~/.cache/emacs/"))

(defmacro expand-dot-file (file)
  "Expand FILE from dot directory."
  `(expand-file-name ,file ,(locate-user-emacs-file "../dot/")))

(when (native-comp-available-p)
  (startup-redirect-eln-cache (expand-cache-file "eln/")))

(when init-file-debug
  (profiler-start 'cpu+mem))

(menu-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(scroll-bar-mode 0)
