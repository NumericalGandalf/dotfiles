(when (native-comp-available-p)
  (when-let ((cache-dir
              (cond ((eq system-type 'gnu/linux)
                     (file-truename
                      (expand-file-name
                       "~/.cache/emacs/eln/"))))))
    (startup-redirect-eln-cache cache-dir)))

(require 'server)
(unless (or (server-running-p) (daemonp))
  (server-start))

(setq package-enable-at-startup nil)

(defun display-startup-echo-area-message ())
(setq inhibit-startup-message t
      server-client-instructions nil)

(menu-bar-mode 0)
