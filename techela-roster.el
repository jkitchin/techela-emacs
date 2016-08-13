;;; techela-roster.el --- functions for handling the roster.
;; techela-roster.el - update course-admin with roster

;; we need to get new students and dropped students,


;;; Commentary:
;; The roster is an org-file. Each headline that has an EMAIL property. Archive,
;; or delete students who have dropped the course. The headline is considered
;; the student name.


;;; Code:

(defvar tq-roster-file nil
  "Derived location of the roster.org file.")


(defun tq-roster-data ()
  "Return a data structure of userids and names from roster.org
The data structure is
 (userid
 :name Full name
 :email userid@somewhere.edu
 :position (point))

Use it like this:
 (plist-get (cdr (assoc \"user@domain.com\" (tq-roster-data))) :name)."
  (with-current-buffer
      (find-file-noselect tq-roster-file) 
    (org-map-entries
     (lambda ()
       (list
	(org-entry-get (point) "EMAIL")
	:name (nth 4 (org-heading-components))
	:email (org-entry-get (point) "EMAIL")
	:position (point)))
     "EMAIL={.}")))


(defun tq-get-userids ()
  "Return list of userids from a roster file."
  (mapcar #'car (tq-roster-data)))


(defun tq-update-git-roster ()
  "Update the list of students in conf/students.conf group.

You run this after you have modified the roster.org file by
adding or removing students."
  (interactive)
  (let* ( ;; known students in git
	 (student-conf-file (expand-file-name
			     "conf/students.conf"
			     tq-gitolite-admin-dir))
	 (userids-in-conf (with-temp-buffer
			    (insert-file-contents student-conf-file)
			    (split-string
			     (nth 1 (split-string (buffer-string) "=")) " " t)))
	 (key-dir (file-name-as-directory
		   (expand-file-name "keydir"
				     tq-gitolite-admin-dir)))
	 
	 (dropped-students (-difference userids-in-conf (tq-get-userids)))
	 (new-students (-difference (tq-get-userids) userids-in-conf)))

    ;; For dropped students we need to remove the userid.pub file
    (when dropped-students
      (dolist (userid dropped-students nil)
	(let ((user-pub-key (format "%s.pub" userid)))
	  (with-current-directory
	   key-dir
	   (if (file-exists-p user-pub-key)
	       (progn
		 (mygit (format "git rm %s" user-pub-key))
		 (mygit (format
			 "git commit %s -m \"deleted %s. %s dropped the class\""
			 user-pub-key user-pub-key userid)))
	     (warn "%s not found for %s" user-pub-key userid))))))
    
    (with-current-directory
     tq-gitolite-admin-dir
     (mygit "git add roster.org")
     (mygit "git commit roster.org -m \"Updated roster.\"")
     (mygit "git push"))

    ;; write out new conf file
    (with-temp-file student-conf-file
      (insert "@students = " (mapconcat 'identity (tq-get-userids) " ")))

    ;; Now we should commit that change, and push it
    (with-current-directory
     (file-name-directory student-conf-file)
     (mygit (format "git commit students.conf -m \"updated the students.conf.

Dropped: %s
Added: %s
\"" dropped-students new-students))
     (mygit "git push")))
  (message "Roster updated in the git repo."))


;; * SSH key utilities
(defun tq-have-user-pubkey-p (userid)
  "Return whether we have the ssh.pub key for USERID."
  (interactive
   (list
    (completing-read
     "Userid: "
     (mapcar (lambda (x)
	       (cons (format "%s | %s"
			     (plist-get (cdr x) :name)
			     (plist-get (cdr x) :email))
		     (car x)))
	     (tq-roster-data))
     nil t)))

  (with-current-directory
   (expand-file-name
    "keydir"
    tq-gitolite-admin-dir)
   (file-exists-p (format "%s.pub" userid))))


(defun tq-check-pub-keys ()
  "Generate a buffer showing which students are missing ssh.pub keys.
Create a link to email them about it."
  (interactive)
  (switch-to-buffer "*ta pub keys*")
  (erase-buffer)
  (insert "#+TITLE: Check if we have ssh.pub keys for users\n\n")
  (dolist (userid (sort (tq-get-userids) 'string-lessp))
    (unless (tq-have-user-pubkey-p userid)
      (insert
       (format
	"%15s missing  %s\n"
	(format "[[elisp:(tq-open-roster-to-user \"%s\")][%s]]"
		userid
		userid)
	(format "[[elisp:(progn (tq-email \"%s@andrew.cmu.edu\")(message-goto-subject)(insert \" Missing ssh pub key\")(message-goto-body)(insert \"Dear %s,\\n\\nI need you to run M-x techela-register to generate and send my your ssh.pub key. Please see me or the TAs if you need help.\\n\\nThanks,\\nProfessor Kitchin\\n\"))][Email %s]]"
		userid
		(plist-get (cdr (assoc userid (tq-roster-data))) :name) userid)))))
  (org-mode))



;; * Open roster and do things interactively
(defvar tq-roster-marked nil
  "Store marked candidates in `tq-roster'.")


(defun tq-roster-mark-candidate () 
  "Add current candidate to `tq-roster-marked'.
If candidate is already in, remove it."
  (interactive) 
  (let ((cand (or (assoc ivy--current (ivy-state-collection ivy-last))
		  ivy--current)))
    (if (-contains? tq-roster-marked cand)
	;; remove it from the marked list
	(setq tq-roster-marked
	      (-remove-item cand tq-roster-marked))
      
      ;; add to list
      (setq tq-roster-marked
	    (append tq-roster-marked (list cand))))) 
  (ivy-next-line))


(defun tq-roster-show-marked-candidates ()
  "Show marked candidates."
  (interactive) 
  (setf (ivy-state-collection ivy-last) tq-roster-marked)
  (setf (ivy-state-preselect ivy-last) ivy--current)
  (ivy--reset-state ivy-last))


(defun tq-roster-show-all ()
  "Show all the candidates."
  (interactive)
  (setf (ivy-state-collection ivy-last)
	(tq-roster-candidates)) 
  (ivy--reset-state ivy-last))


(defun tq-roster-select-all-toggle ()
  "Toggle the marked state of all candidates."
  (interactive)
  (if tq-roster-marked
      (setq tq-roster-marked '())
    (setq tq-roster-marked
	  (ivy-state-collection ivy-last))))


(defvar tq-roster-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<SPC>") 'tq-roster-mark-candidate)
    (define-key map (kbd "C-,") 'tq-roster-show-marked-candidates)
    (define-key map (kbd "C-.") 'tq-roster-show-all) 
    (define-key map (kbd "C-a") 'tq-roster-select-all-toggle)
    map)
  "A keymap for `tq-roster'.")


(defun tq-roster-candidates ()
  "Return list of candidates for tq-roster functions."
  (mapcar (lambda (x)
	    (cons
	     (format "%-40s|%s"
		     (plist-get (cdr x) :name)
		     (car x))
	     x))
	  (tq-roster-data)))


(defun tq-roster ()
  "Open the roster in an ivy selection buffer."
  (interactive)
  (setq tq-roster-marked '())
  (ivy-read "Student: " (tq-roster-candidates)
	    :require-match t
	    :keymap tq-roster-keymap
	    :caller 'tq-roster
	    :action
	    '(1
	      ("e" tq-roster-email
	       "Email")
	      ("o" tq-roster-open-roster-to-student
	       "Open roster."))))


(defun tq-roster-transformer (s)
  "Make entry red if it is marked."
  (if (-contains?
       (if (listp (car tq-roster-marked))
	   (mapcar 'car tq-roster-marked)
	 tq-roster-marked)
       s)
      (propertize s 'face '(:foreground "red"))
    (propertize s 'face nil)))

(ivy-set-display-transformer
 'tq-roster
 'tq-roster-transformer )


(defun tq-roster-open-roster-to-student (x)
  "Open roster to the student in X.
X is a element from `tq-roster-candidates'."
  (find-file tq-roster-file)
  (goto-char (plist-get (cdr x) :position))
  (show-subtree))


(defun tq-roster-email (x)
  "Email student X, unless `tq-roster-marked' is non-nil." 
  (let ((email (if tq-roster-marked
  		   (mapconcat (lambda (x) 
  				(plist-get (cddr x) :email))
  			      tq-roster-marked
  			      ",")
  		 (plist-get (cdr x) :email)))) 
    (compose-mail-other-window)
    (message-goto-to)
    (insert email)
    (message-goto-subject)
    (insert (format "[%s] " tq-current-course))))


(provide 'techela-roster)

;;; techela-roster.el ends here
