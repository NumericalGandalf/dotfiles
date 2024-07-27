(defun native-comp-redirect ()
  "Redirect native comp eln cache."
  (when-let ((cache-dir
              (cond ((eq system-type 'gnu/linux)
                     (file-truename
                      (expand-file-name
                       "~/.cache/emacs/eln/"))))))
    (startup-redirect-eln-cache cache-dir)))

(when (native-comp-available-p)
  (native-comp-redirect))

(require 'server)
(unless (or (server-running-p) (daemonp))
  (server-start))

(setq package-enable-at-startup nil)

(defun display-startup-echo-area-message ())
(setq server-client-instructions nil)

(menu-bar-mode 0)
