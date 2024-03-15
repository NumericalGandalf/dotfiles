(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
    (expand-file-name "/var/eln-cache/" user-emacs-directory)))
