(general-define-key
 "C-S-N" 'next-buffer
 "C-S-P" 'previous-buffer)

(general-define-key
 :prefix "C-x"
 "C-c" 'save-buffers-kill-emacs
 "C-b" 'ibuffer-other-window)

(general-define-key
 :prefix "C-c"
 "b r" 'revert-buffer-quick

 "f r" 'recentf
 "f l" 'find-library

 "z l" 'load-file
 "z f" 'font-load

 "y p" 'elpaca-manager

 "h f" 'describe-face
 "h g" 'customize-group
 "h t" 'describe-theme

 "o ." (lambda ()
         (interactive)
         (find-file (file-truename user-init-file)))
 "o ," (lambda ()
         (interactive)
         (find-file (rc-cache)))
 "o /" (lambda ()
         (interactive)
         (find-file (rc-dots))))

(provide 'keybindings)
