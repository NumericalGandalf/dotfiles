(require 'server)
(setq server-stop-automatically 'empty)
(unless (server-running-p) (server-start)) 

(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
    (expand-file-name "/var/eln-cache/" user-emacs-directory)))
