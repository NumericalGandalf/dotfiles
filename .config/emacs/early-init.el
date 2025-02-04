(defmacro expand-cache-file (&optional file)
  "Expand FILE from cache directory."
  `(expand-file-name ,(or file "./") "~/.cache/emacs/"))

(when (native-comp-available-p)
  (startup-redirect-eln-cache (expand-cache-file "eln/")))

(when init-file-debug
  (profiler-start 'cpu+mem))

(menu-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(scroll-bar-mode 0)
