(when (and (boundp 'native-comp-eln-load-path)
	   (eq system-type 'gnu/linux))
  (startup-redirect-eln-cache "~/.cache/emacs/eln/"))

(require 'server)
(unless (or (server-running-p) (daemonp))
  (server-start))

(setq package-enable-at-startup nil)

(defun display-startup-echo-area-message ())
(setq server-client-instructions nil)

(menu-bar-mode 0)
