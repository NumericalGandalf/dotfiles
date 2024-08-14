(when (native-comp-available-p)
  (when-let ((posix-dir
              (file-truename
               (expand-file-name "~/.cache/emacs/eln/")))
             (cache-dir
              (pcase system-type
                ('gnu/linux posix-dir)
                ('gnu/kfreebsd posix-dir))))
    (startup-redirect-eln-cache cache-dir)))

(require 'server)
(unless (or (server-running-p) (daemonp))
  (server-start))

(setq package-enable-at-startup nil)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
