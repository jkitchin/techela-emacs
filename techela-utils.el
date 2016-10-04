;;; techela-utils.el --- utilities


;;; Commentary:
;;

;;; Code:

(defun tq-check-internet ()
  "Use ping to see if the internet is available"
  (if (executable-find "ping")
      (cond
       ((string= system-type "windows-nt")
	(unless (= 0 (shell-command
		      (format "ping -n 1 www.cmu.edu")))
	  (message-box "Unable to contact www.cmu.edu.
Check your internet connection."))
	t)

       ((string= system-type "darwin")
	(unless (= 0
		   (shell-command
		    (format "ping -c 1 www.cmu.edu")))
	  (message-box "Unable to contact www.cmu.edu.
Check your internet connection")
	  (error "Unable to contact www.cmu.edu. Check your internet connection."))
	t)

       (t ;; all other systems
	(unless (= 0
		   (shell-command (format "ping -c 1 www.cmu.edu")))
	  (message-box "Unable to contact www.cmu.edu.
Check your internet connection")
	  (error "Unable to contact www.cmu.edu. Check your internet connection."))
	t))
    ;; no ping found !
    (message "You have no ping executable! I cannot check for internet connectivity.")))


(defun tq-log (format-string &rest args)
  "Log a message to *techela log*.  Same syntax as `message'.
The first argument is a format control string, and the rest are data
to be formatted under control of the string.  See `format' for details.

Note: Use (tq-log \"%s\" VALUE) to print the value of expressions and
variables to avoid accidentally interpreting `%' as format specifiers.
Argument FORMAT-STRING format string.
Optional argument ARGS extra arguments."
  (with-current-buffer (get-buffer-create "*techela log*")
    (goto-char (point-max))
    (insert "\n")
    (insert (apply 'format format-string args))))


(defun tq-insert-system-info ()
  "Create a SYSTEM-INFO file containing system info." 
  (with-temp-file "SYSTEM-INFO"
    (insert "Created on: " (current-time-string) "\n")
    (insert (format "Name: %s\n" user-full-name))
    (insert (format "Userid = %s\n" (gethash "user-mail-address" (tq-read-user-data))))
    (insert (format "Email: %s\n" user-mail-address))
    (insert "System name: " (system-name))
    (insert (format "\n%s" system-type))
    ;; some information about ip addresses and mac address
    (insert (shell-command-to-string ifconfig-program))))


;; http://www.gnu.org/software/emacs/manual/html_node/eintr/Files-List.html
(defun files-in-below-directory (directory)
  "List the .org files in DIRECTORY and in its sub-directories." 
  (f-files directory (lambda (f) (f-ext? f ".org")) t))


;;;###autoload
(defun tq-search (regexp)
  "Search all of the course files using `multi-occur-in-matching-buffers' for REGEXP.
Opens all course files, then does the search."
  (interactive (list (read-regexp "Regexp: ")))

  (let ((org-files (f-files tq-course-directory (lambda (f) (string= "org" (f-ext f))) t)))
    ;; open all the files so we can use multi-occur
    (dolist (f org-files)
      (find-file-noselect f))
    (multi-occur-in-matching-buffers ".*.org" regexp)))


;;;###autoload
(defun tq-toc ()
  "Generate a table of contents from the syllabus."
  (interactive)
  (let ((*org-files* '()))
    (set-buffer (find-file-noselect tq-course-syllabus))
    (org-open-link-from-string "[[#schedule]]")
    (save-restriction
      (org-narrow-to-subtree)
      (org-element-map (org-element-parse-buffer) 'link
	(lambda (link)
	  (let ((type (nth 0 link))
		(plist (nth 1 link)))
	    (when (equal (plist-get plist ':type) "file")
	      (add-to-list '*org-files*
			   (expand-file-name
			    (plist-get plist :path))
			   t))))))

    (switch-to-buffer "*techela toc*")
    (erase-buffer)
    (insert "#+TITLE: Table of Contents\n")
    (dolist (f *org-files*)
      (with-current-buffer (find-file-noselect f)
	(org-map-entries
	 (lambda ()
	   (let* ((components (org-heading-components))
		  (p (point))
		  (h (nth 4 components)))

	     ;; remove links in headlines
	     (setq h (replace-regexp-in-string "\\[" "" h))
	     (setq h (replace-regexp-in-string "\\]" "" h))

	     (with-current-buffer   (get-buffer-create "*techela toc*")
	       (dotimes (i (nth 0 components))
		 (insert "*"))

	       (insert " "
		       (format "[[elisp:(progn (find-file \"%s\")(goto-char %s))][%s]]"
			       f	; the filename
			       p	; where we are in the file
			       h )	;the headline
		       "\n")))))))

    (switch-to-buffer "*techela toc*")
    (org-mode)))


;;;###autoload
(defun tq-index ()
  "Generate a temporary index buffer from the course files."
  (interactive)
  (let ((*index-links*)
	(*initial-letters*)
	(org-files (f-files tq-course-directory (lambda (f) (string= (f-ext f) "org")) t)))
    ;; get links
    (dolist (f org-files)
      (find-file f)
      (read-only-mode -1)
      (org-element-map (org-element-parse-buffer) 'link
	(lambda (link)
	  (let ((type (nth 0 link))
		(plist (nth 1 link)))

	    (when (equal (plist-get plist ':type) "index")
	      (add-to-list
	       '*index-links*
	       (cons (plist-get plist :path)
		     (format
		      "[[elisp:(progn (switch-to-buffer \"%s\")(goto-char %s))][%s]] (%s)"

		      (current-buffer)			   ;; buffer name
		      (plist-get plist :begin)		   ;; position of link
		      (save-excursion
			(goto-char (plist-get plist :begin))
			(if (thing-at-point 'sentence)
			    (replace-regexp-in-string "\n" "" (thing-at-point 'sentence))
			  "link"))
		      (file-name-nondirectory (buffer-file-name))))))))))
    (setq *index-links*  (cl-sort *index-links* 'string-lessp :key 'car))

    ;; now first letters
    (dolist (link *index-links*)
      (add-to-list '*initial-letters* (substring (car link) 0 1) t))
    
    (switch-to-buffer (get-buffer-create "*index*"))
    (org-mode)
    (erase-buffer)
    (insert "#+TITLE: Index\n\n")
    (dolist (letter *initial-letters*)
      (insert (format "* %s\n" (upcase letter)))
      ;; now process the links
      (while (and *index-links* (string= letter (substring (car (car *index-links*)) 0 1)))
	(let ((link (pop *index-links*)))
	  (insert (format "%s %s\n\n" (car link) (cdr link)))))))
  (switch-to-buffer "*index*"))

;;;###autoload
(defun tq-increase-text-size ()
  "Increase text size."
  (interactive)
  (set-face-attribute 'default nil :height
		      (truncate (* 1.1 (face-attribute 'default :height)))))

;;;###autoload
(defun tq-decrease-text-size ()
  "Decrease text size."
  (interactive)
  (set-face-attribute 'default nil :height
		      (truncate (* 0.9 (face-attribute 'default :height)))))

;;;###autoload
(defun tq-present ()
  "Set font size larger and set latex fragment size larger."
  (interactive)
  (hl-line-mode)
  (global-linum-mode)
  (set-face-attribute 'default nil :height 180)
  (plist-put org-format-latex-options :scale 1.7))


(global-set-key (kbd "C--") 'tq-decrease-text-size)
(global-set-key (kbd "C-=") 'tq-increase-text-size)

;;;###autoload
(defun tq-clean-line-endings ()
  "Remove ^M from lines in buffer.
This seems to happen to some students because of the mix of
dos/mac and unix line endings."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "" nil t)
    (replace-match "")))


(defun swap (LIST el1 el2)
  "In LIST swap indices EL1 and EL2 in place.
LIST is modified."
  (let ((tmp (elt LIST el1)))
    (setf (elt LIST el1) (elt LIST el2))
    (setf (elt LIST el2) tmp)))


(defun shuffle (LIST)
  "Shuffle the elements in LIST so they are in random order.
Shuffling is done in place."
  (loop for i in (reverse (number-sequence 1 (1- (length LIST))))
	do (let ((j (random (+ i 1))))
	     (swap LIST i j)))
  LIST)


(defun counts (list)
  "Return an alist of counts for each element of the list.
   ((element . count))"
  (let ((counts '())
	place)
    
    (dolist (el list)
      (setq place (assoc el  counts))
      (if place
	  (incf (cdr place))
	(push (cons el 1) counts)))
    counts))


;;;###autoload
(defun tq-describe ()
  "Open a buffer with information about the setup for techela."
  (interactive)
  (switch-to-buffer (get-buffer-create "*techela describe*"))
  (insert
   (mustache-render
    "Name: {{name}}
Userid: {{userid}}
Email: {{email}}
System: {{system-type}}
Window system: {{window-system}}

temporary file directory: {{tempdir}}
~/: {{home}}
git located at: {{git}}
ssh located at: {{ssh}}
python located at: {{python}}

* techela-ssh-config
{{tq-ssh-config}}

* ssh pub key
{{ssh-pub}}
"
    (ht ("name" user-full-name)
	("userid" (gethash "user-mail-address" (tq-read-user-data)))
	("email" user-mail-address)
	("system-type" (format "%s" system-type))
	("window-system" (format "%s" window-system))
	("tempdir" temporary-file-directory)
	("home" (expand-file-name "~/"))
	("git" (executable-find "git"))
	("ssh" (executable-find "ssh"))
	("python" (executable-find "python"))
	("tq-ssh-config" (with-temp-buffer
			   (insert-file-contents
			    (expand-file-name
			     (format "~/techela/%s/techela-ssh-config"
				     (techela-course-label tq-current-course))))
			   (buffer-string)))
	("ssh-pub" (with-temp-buffer
		     (insert-file-contents
		      (expand-file-name
		       (format "~/techela/%s/%s.pub"
			       (techela-course-label tq-current-course)
			       (gethash "user-mail-address" (tq-read-user-data)))))
		     (buffer-string))))))
  (org-mode))

(provide 'techela-utils)

;;; techela-utils.el ends here
