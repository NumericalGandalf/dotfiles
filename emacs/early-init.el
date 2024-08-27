(when (native-comp-available-p)
  (when-let ((posix-dir
              (file-truename
               (expand-file-name "~/.cache/emacs/eln/")))
             (cache-dir
              (pcase system-type
                ('gnu/linux posix-dir)
                ('gnu/kfreebsd posix-dir))))
    (startup-redirect-eln-cache cache-dir)))

(setq package-enable-at-startup nil)

(setq load-prefer-newer t)

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
