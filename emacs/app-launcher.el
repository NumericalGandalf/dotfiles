;; Taken from https://github.com/SebastienWae/app-launcher

(require 'xdg)
(require 'cl-seq)

(defcustom app-launcher-apps-directories
  (mapcar (lambda (dir) (expand-file-name "applications" dir))
	  (cons (xdg-data-home) (xdg-data-dirs)))
  "Directories in which to search for applications (.desktop files)."
  :type '(repeat directory))

(defcustom app-launcher--annotation-function #'app-launcher--annotation-function-default
  "Define the function that genereate the annotation for each completion choices."
  :type 'function)

(defcustom app-launcher--action-function #'app-launcher--action-function-default
  "Define the function that is used to run the selected application."
  :type 'function)

(defvar app-launcher--cache nil
  "Cache of desktop files data.")

(defvar app-launcher--cache-timestamp nil
  "Time when we last updated the cached application list.")

(defvar app-launcher--cached-files nil
  "List of cached desktop files.")

(defvar app-launcher--curr-max-name-len 0
  "Current maximum length of an application name.")

(defun app-launcher--name-len (name)
  "Return the length of NAME and update `app-launcher--curr-max-name-len'."
  (let ((len (length name)))
    (when (> len app-launcher--curr-max-name-len)
      (setq app-launcher--curr-max-name-len len))
    len))

(defun app-launcher-list-desktop-files ()
  "Return an alist of all Linux applications.
Each list entry is a pair of (desktop-name . desktop-file).
This function always returns its elements in a stable order."
  (let ((hash (make-hash-table :test #'equal))
	result)
    (dolist (dir app-launcher-apps-directories)
      (when (file-exists-p dir)
	(let ((dir (file-name-as-directory dir)))
	  (dolist (file (directory-files-recursively dir ".*\\.desktop$" nil nil t))
	    (let ((id (subst-char-in-string ?/ ?- (file-relative-name file dir))))
	      (when (and (not (gethash id hash)) (file-readable-p file))
		(push (cons id file) result)
		(puthash id file hash)))))))
    result))

(defun app-launcher-parse-files (files)
  "Parse the .desktop files to return usable informations."
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (entry files hash)
      (let ((file (cdr entry)))
	(with-temp-buffer
	  (insert-file-contents file)
	  (goto-char (point-min))
	  (let ((start (re-search-forward "^\\[Desktop Entry\\] *$" nil t))
		(end (re-search-forward "^\\[" nil t))
		(visible t)
		name comment exec path)
	    (catch 'break
	      (unless start
		(message "Warning: File %s has no [Desktop Entry] group" file)
		(throw 'break nil))

	      (goto-char start)
	      (when (re-search-forward "^\\(Hidden\\|NoDisplay\\) *= *\\(1\\|true\\) *$" end t)
		(setq visible nil))
	      (setq name (match-string 1))

	      (goto-char start)
	      (unless (re-search-forward "^Type *= *Application *$" end t)
		(throw 'break nil))
	      (setq name (match-string 1))

	      (goto-char start)
	      (unless (re-search-forward "^Name *= *\\(.+\\)$" end t)
		(push file counsel-linux-apps-faulty)
		(message "Warning: File %s has no Name" file)
		(throw 'break nil))
	      (setq name (match-string 1))

	      (goto-char start)
	      (when (re-search-forward "^Comment *= *\\(.+\\)$" end t)
		(setq comment (match-string 1)))

	      (goto-char start)
	      (when (re-search-forward "^Path *= *\\(.+\\)$" end t)
		(setq path (match-string 1)))

	      (goto-char start)
	      (unless (re-search-forward "^Exec *= *\\(.+\\)$" end t)
		;; Don't warn because this can technically be a valid desktop file.
		(throw 'break nil))
	      (setq exec (match-string 1))

	      (goto-char start)
	      (when (re-search-forward "^TryExec *= *\\(.+\\)$" end t)
		(let ((try-exec (match-string 1)))
		  (unless (locate-file try-exec exec-path nil #'file-executable-p)
		    (throw 'break nil))))

	      (puthash name
		       (list (cons 'file file)
			     (cons 'exec exec)
                             (cons 'path path)
			     (cons 'comment comment)
			     (cons 'visible visible)
                             (cons 'len (app-launcher--name-len name)))
		       hash))))))))

(defun app-launcher-list-apps ()
  "Return list of all Linux .desktop applications."
  (let* ((new-desktop-alist (app-launcher-list-desktop-files))
	 (new-files (mapcar 'cdr new-desktop-alist)))
    (unless (and (equal new-files app-launcher--cached-files)
		 (null (cl-find-if
			(lambda (file)
			  (time-less-p
			   app-launcher--cache-timestamp
			   (nth 5 (file-attributes file))))
			new-files)))
      (setq app-launcher--cache (app-launcher-parse-files new-desktop-alist))
      (setq app-launcher--cache-timestamp (current-time))
      (setq app-launcher--cached-files new-files)))
  app-launcher--cache)

(defun app-launcher--annotation-function-default (choice)
  "Default function to annotate the completion choices."
  (let  ((str (cdr (assq 'comment (gethash choice app-launcher--cache))))
         (len (cdr (assq 'len (gethash choice app-launcher--cache)))))
    (if str
        (concat (make-string (+ 2 (if icomplete-mode
                                      0
                                    (- app-launcher--curr-max-name-len len)))
                             ?\s)
                (propertize str 'face 'completions-annotations))
      "")))

(defun app-launcher--action-function-default (selected)
  "Default function used to run the selected application."
  (let* ((exec (cdr (assq 'exec (gethash selected app-launcher--cache))))
	 (command (let (result)
		    (dolist (chunk (split-string exec " ") result)
		      (unless (or (equal chunk "%U")
				  (equal chunk "%F")
				  (equal chunk "%u")
				  (equal chunk "%f"))
			(setq result (concat result chunk " "))))))
         (default-directory
          (or (cdr (assq 'path (gethash selected app-launcher--cache)))
              default-directory)))
    (call-process-shell-command command nil 0 nil)))

(defun app-launcher-run-app (&optional prefix)
  "Launch an application installed on your machine.
When PREFIX is non-nil, ignore NoDisplay property in *.desktop files."
  (interactive "P")
  (let* ((candidates (app-launcher-list-apps))
	 (result (completing-read
		  "Run app: "
		  (lambda (str pred flag)
		    (if (eq flag 'metadata)
			'(metadata
			  (annotation-function . (lambda (choice)
						   (funcall
						    app-launcher--annotation-function
						    choice))))
		      (complete-with-action flag candidates str pred)))
		  (lambda (x &optional y)
		    (if prefix
			t
		      (cdr (assq 'visible y))))
		  t nil 'app-launcher nil nil)))
    (funcall app-launcher--action-function result)))

(defun app-launcher-frame (&optional prefix)
  "Create dedicated frame for `app-launcher-run-app'.
When PREFIX is non-nil, ignore NoDisplay property in *.desktop files."
  (interactive "P")
  (let* ((height 21)
         (vertico-count (1- height)))
    (with-selected-frame (make-frame `((name . "app-launcher.el")
                                       (height . ,height)
                                       (width . ,(* height 5))
                                       (minibuffer . only)))
      (unwind-protect
          (app-launcher-run-app prefix)
        (delete-frame)))))
