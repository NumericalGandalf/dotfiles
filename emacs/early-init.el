(defmacro rc/require (feature)
  "Load FEATURE from the config `lisp' directory."
  `(load (locate-user-emacs-file (symbol-name ,feature)) t t))

(rc/require 'rc)

(when (native-comp-available-p)
  (startup-redirect-eln-cache (rc/cache "eln/"))
  (setq native-comp-async-report-warnings-errors 'silent))

(when init-file-debug
  (setq debug-on-error t)
  (profiler-start 'cpu+mem))

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
