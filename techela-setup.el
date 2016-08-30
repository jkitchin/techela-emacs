;;; techela-setup.el --- setup techela courses 

;;; Commentary:
;; Here we create necessary scripts to run ssh and git. We generate the ssh keys
;; and send them to the instructor.

;; Students and TA's should run `tq-register'. 

;; Instructors should run `tq-setup-course' to setup the course locally and
;; remotely. This only needs to be run once.

(require 'json)
(require 'mustache)
(require 'ht)


;;; Code:

;; * Student setup
(defun tq-read-user-data ()
  "Read and return the data in `tq-user-data-file'."
  (if (file-exists-p tq-user-data-file)
      (let ((json-object-type 'hash-table))
	(json-read-file tq-user-data-file))
    ;; no file exists, return an empty hash
    (make-hash-table :test 'equal)))


(defun tq-write-user-data (data)
  "Write DATA to `tq-user-data-file'.
DATA should be obtained and modified from `tq-read-user-data'."
  (with-temp-file tq-user-data-file
    (insert (json-encode-hash-table data))))


(defun tq-setup-user ()
  "Get and store user data like name and email.
Configure git with this information where needed."

  (unless (executable-find "git")
    (error "I cannot find git.  You cannot use techela"))

  ;; Full name
  (let ((data (tq-read-user-data)))
    (unless (gethash "user-full-name" data)
      (puthash "user-full-name" (read-from-minibuffer "Enter your full name: ") data)
      (tq-write-user-data data)))

  ;; email address
  (let ((data (tq-read-user-data)))
    (unless (gethash "user-mail-address" data)
      (puthash "user-mail-address" (read-from-minibuffer "Enter your email address: ") data)
      (tq-write-user-data data))

    (when (string= "" (shell-command-to-string "git config --global user.name"))
      (shell-command (format "git config --global user.name \"%s\""
			     (gethash "user-full-name" data))))

    (when (string= "" (shell-command-to-string "git config --global user.email"))
      (shell-command (format "git config --global user.email %s"
			     (gethash "user-mail-address" data))))
    
    ;; Append a line to the ~/.authinfo for authentication with mail
    ;; Users will be prompted for their andrew password
    (unless (file-exists-p (expand-file-name "~/.authinfo"))
      (with-temp-file (expand-file-name "~/.authinfo")))

    (let ((contents (with-temp-buffer
		      (insert-file-contents
		       (expand-file-name "~/.authinfo"))
		      (buffer-string)))
	  user-id)
      (setq user-id (car (split-string (gethash "user-mail-address" data) "@")))
      (unless (string-match "smtp\.andrew\.cmu\.edu" contents)
	(with-temp-file (expand-file-name "~/.authinfo")
	  (when contents (insert contents))
	  (goto-char (point-max))
	  (insert
	   (format
	    "\nmachine smtp.andrew.cmu.edu port 587 login %s" user-id))))))

  (when (string= "" (shell-command-to-string "git config --global push.default"))
    (shell-command "git config --global push.default matching"))

  ;; how to send mail
  (when (equal send-mail-function 'sendmail-query-once)
    (setq send-mail-function 'smtpmail-send-it))

  ;; this will clobber any user settings. For new students these are
  ;; not likely to be set. I am not sure how to handle this generally.
  (setq smtpmail-smtp-server "smtp.andrew.cmu.edu"
	smtpmail-smtp-service 587
	smtpmail-starttls-credentials '(("smtp.andrew.cmu.edu" 587 nil nil))
	smtpmail-stream-type nil
	starttls-use-gnutls t
	starttls-gnutls-program "gnutls-cli")

  (unless (and (boundp 'mail-host-address) mail-host-address)
    (setq mail-host-address "andrew.cmu.edu"))
  
  (message "Done with user information."))


(defun tq-setup-ssh ()
  "Setup ssh for use with techela.
Make sure ssh is available. Generate ssh key, config and wrapper
scripts. Email key to instructor."
  
  (unless (executable-find "ssh")
    (error "I cannot find ssh.  You cannot use techela"))

  ;; check for tq-root-directory
  (unless (file-exists-p tq-root-directory)
    (make-directory tq-root-directory t))
  
  ;; here we look for the existence of tq-root-directory/userid.pub which is a
  ;; an ssh key. We do not replace these since they are registered.
  (unless (file-exists-p (expand-file-name
			  (concat
			   (gethash "user-mail-address" (tq-read-user-data))
			   ".pub")
			  tq-root-directory))
    ;; we make one with no password
    (shell-command (format "ssh-keygen -t rsa -f \"%s\" -N \"\""
			   (expand-file-name
			    (gethash "user-mail-address" (tq-read-user-data))
			    tq-root-directory))))

  ;; Now we make the ssh config file. 
  (with-temp-file (expand-file-name
		   "techela-ssh-config"
		   tq-root-directory)
    (insert (mustache-render
	     "Host {{host}}
  User {{course}}
  IdentityFile \"{{identify-file}}\"
  StrictHostKeyChecking no
  UserKnownHostsFile /dev/null\n"
	     (ht ("host" (nth 1 (split-string
				 (techela-course-techela-server tq-current-course) "@")))
		 ("course" (symbol-name (techela-course-label  tq-current-course)))
		 ("identify-file" (expand-file-name
				   (gethash "user-mail-address" (tq-read-user-data))
				   tq-root-directory))))))

  ;; This is the executable ssh script
  (with-temp-file (expand-file-name
		   "techela_ssh"
		   tq-root-directory)
    (insert
     (format "#!/bin/bash
# custom ssh for running git in batch mode for techela with the user-key
exec ssh -F \"%s\" -o \"BatchMode yes\" \"$@\"
# end" (expand-file-name "techela-ssh-config" tq-root-directory))))

  (set-file-modes (expand-file-name
		   "techela_ssh"
		   tq-root-directory)
		  #o755)

  ;; This is an executable git script
  (with-temp-file (expand-file-name
		   "techela_git"
		   tq-root-directory)
    (insert
     (format "#!/bin/bash
# custom git for techela with the user-key
ENV GIT_SSH=\"%s\" git $@
# end" (expand-file-name "techela_ssh" tq-root-directory))))
  
  (set-file-modes (expand-file-name
		   "techela_git"
		   tq-root-directory)
		  #o755)

  ;; Finally setup and send the email
  ;; now create an email and send the key to the instructor
  (compose-mail)
  ;; Replace whatever emacs puts in with the user email.
  (message-goto-from)
  (message-beginning-of-line)
  (kill-line)
  (insert (gethash "user-mail-address" (tq-read-user-data)))
  (message-goto-to)
  (insert (techela-course-instructor-email tq-current-course))
  (message-goto-subject)
  (insert (format "[%s] %s pubkey" (techela-course-label tq-current-course)
		  (gethash "user-mail-address" (tq-read-user-data))))
  (mml-attach-file (expand-file-name
		    (concat (gethash "user-mail-address"
				     (tq-read-user-data)) ".pub")
		    tq-root-directory))
  (message-goto-body)
  ;; let us get some user/computer information
  (tq-insert-system-info)
  (insert (with-temp-buffer
	    (insert-file-contents "SYSTEM-INFO")
	    (buffer-string)))
  (delete-file "SYSTEM-INFO")

  (message-send-and-exit))


(defun tq-register (course)
  "Register for COURSE.
COURSE is a struct representing the course. This sets up your
local machine and emails the instructor your ssh pub key. You
cannot access the course until you get an email confirmation
back.

This is for students and TAs."
  (interactive
   (list
    (prog2
	(tq-check-internet)
	(let ((courses (tq-get-courses)))
	  (cdr (assoc (completing-read
		       "Course name: " courses)
		      courses))))))
  
  ;; Set this for the current session
  (setq tq-current-course course)

  ;; initialize to nil, just in case they were previously set
  (setq tq-root-directory (file-name-as-directory
			   (expand-file-name
			    (symbol-name (techela-course-label course))
			    (expand-file-name tq-user-root))))

  (setq tq-course-directory (expand-file-name "course" tq-root-directory)
	tq-user-data-file (expand-file-name "techela-user-data" tq-root-directory))

  ;; make root directory if needed, including parents
  (unless (file-exists-p tq-root-directory)
    (make-directory tq-root-directory t))
  
  (tq-setup-user)
  (tq-setup-ssh)
  (message "Please wait for further instructions from the instructor."))


;; * Admin course setup functions
(defun tq-setup-remote-course (course)
  "Setup COURSE on the remote server and locally.
COURSE should be a struct from `tq-get-courses'. You should have
password free communication with the techela server using ssh
keys."
  (interactive
   (list
    (prog2
	(tq-check-internet)
	(let ((courses (tq-get-courses)))
	  (cdr (assoc (completing-read
		       "Course name: " courses)
		      courses))))))
  
  ;; clone gitolite remotely if needed
  (shell-command (format "ssh %s \"[ ! -d gitolite ] && git clone https://github.com/jkitchin/gitolite\""
			 (techela-course-techela-server course)))
  
  (shell-command (format "ssh %s \"[ ! -d bin ] && mkdir bin\""
			 (techela-course-techela-server course)))

  (let ((user (car (split-string (techela-course-techela-server course) "@"))))
    ;; this is a little hacky but $HOME is not set correctly for ssh on
    ;; techela.cheme.cmu.edu for some reason.
    (shell-command (format "ssh %s \"gitolite/install --to /home/%s/bin\""
			   (techela-course-techela-server course)
			   user)))

  (setq tq-root-directory (expand-file-name (symbol-name (techela-course-label course))
					    tq-user-root))
  
  (unless (file-directory-p tq-root-directory)
    (make-directory tq-root-directory t))

  ;; generate a local admin ssh key if needed
  (unless (file-exists-p (expand-file-name
			  (techela-course-instructor-email course)
			  tq-root-directory))
    (let ((default-directory tq-root-directory))
      (shell-command (format "ssh-keygen -t rsa -f %s -N \"\""
			     (techela-course-instructor-email course)))))
  
  ;; copy admin ssh key over to server if it isn't there
  (unless (= 0 (shell-command (format "ssh %s \"ls %s.pub\""
				      (techela-course-techela-server course)
				      (techela-course-instructor-email course))))
    (shell-command (format "scp %s.pub %s:%s.pub"
			   (expand-file-name
			    (techela-course-instructor-email course) tq-root-directory)
			   (techela-course-techela-server course)
			   (techela-course-instructor-email course))))

  (unless (= 0 (shell-command
		(format "ssh %s \"[ -e .gitolite/conf/gitolite.conf ] && grep %s .gitolite/conf/gitolite.conf\""
			(techela-course-techela-server course)
			(techela-course-instructor-email course))))
    (shell-command (format "ssh %s \"bin/gitolite setup -pk %s.pub\""
			   (techela-course-techela-server course)
			   (techela-course-instructor-email course))))
  (message "Done setting up remote server."))


(defun tq-setup-local-directories (course)
  "Setup our local directories for the COURSE."
  (interactive
   (list
    (prog2
	(tq-check-internet)
	(let ((courses (tq-get-courses)))
	  (cdr (assoc (completing-read
		       "Course name: " courses)
		      courses))))))

  (setq tq-current-course course
	tq-root-directory (expand-file-name
			   (symbol-name (techela-course-label course))
			   tq-user-root)
	tq-course-assignments-directory (expand-file-name
					 "assignments"
					 tq-root-directory)
	tq-course-solutions-directory (expand-file-name
				       "solutions"
				       tq-root-directory)
	tq-course-student-work-directory (expand-file-name
					  "student-work"
					  tq-root-directory)
	tq-course-class-work-directory (expand-file-name
					"class-work"
					tq-root-directory)
	tq-course-directory (expand-file-name
			     "course"
			     tq-root-directory)
	tq-gitolite-admin-dir (expand-file-name
			       "gitolite-admin"
			       tq-root-directory)
	tq-user-data-file (expand-file-name
			   "techela-user-data"
			   tq-root-directory))

  (let ((data (tq-read-user-data)))
    (unless (gethash "user-full-name" data)
      (puthash "user-full-name" (techela-course-instructor course) data)
      (tq-write-user-data data))
    (unless (gethash "user-mail-address" data)
      (puthash "user-mail-address" (techela-course-instructor-email course) data)
      (tq-write-user-data data)))
  
  ;; Setup local directory structure - ~/techela
  (unless (file-directory-p tq-user-root)
    (make-directory tq-user-root t))

  ;; ~/techela/course-label
  (unless (file-directory-p tq-root-directory)
    (make-directory tq-root-directory))

  (with-temp-file (expand-file-name "README.org"
				    tq-root-directory)
    (insert
     (mustache-render (with-temp-buffer (insert-file-contents
					 (expand-file-name
					  "templates/readme.org"
					  (file-name-directory
					   (locate-library "techela"))))
					(buffer-string))
		      (ht ("host" (techela-course-techela-server tq-current-course))
			  ("course-repo" (techela-course-course-repo tq-current-course))
			  ("label" (symbol-name (techela-course-label tq-current-course)))
			  ("techela_ssh" (expand-file-name
					  "techela_ssh"
					  tq-root-directory))
			  ("techela_git" (expand-file-name
					  "techela_git"
					  tq-root-directory))))))

  ;; Now generate ssh-config files to use the key
  (with-temp-file (expand-file-name
		   "techela-ssh-config"
		   tq-root-directory)
    (insert (mustache-render
	     "Host {{host}}
  User {{course}}
  IdentityFile {{identify-file}}
  StrictHostKeyChecking no
  UserKnownHostsFile /dev/null

Host techela
  User {{course}}
  Hostname {{host}}
  IdentityFile {{identify-file}}
  StrictHostKeyChecking no
  UserKnownHostsFile /dev/null\n"
	     (ht ("host" (nth 1 (split-string
				 (techela-course-techela-server tq-current-course)
				 "@")))
		 ("course" (symbol-name (techela-course-label  course)))
		 ("identify-file" (expand-file-name
				   (techela-course-instructor-email course)
				   tq-root-directory))))))

  ;; and the custom ssh and git 
  (with-temp-file (expand-file-name
		   "techela_ssh"
		   tq-root-directory)
    (insert
     (format "#!/bin/bash
# custom ssh for running git in batch mode for techela with the user-key
exec ssh -F \"%s\" -o \"BatchMode yes\" \"$@\"
# end" (expand-file-name "techela-ssh-config" tq-root-directory))))
  
  
  (set-file-modes (expand-file-name
		   "techela_ssh"
		   tq-root-directory)
		  #o755)

  (with-temp-file (expand-file-name
		   "techela_git"
		   tq-root-directory)
    (insert
     (format "#!/bin/bash
# custom git for techela with the user-key
ENV GIT_SSH=%s git $@
# end" (expand-file-name "techela_ssh" tq-root-directory))))

  (set-file-modes (expand-file-name
		   "techela_git"
		   tq-root-directory)
		  #o755)

  ;; assignments
  (unless (file-directory-p tq-course-assignments-directory)
    (make-directory tq-course-assignments-directory t)
    (with-temp-file (expand-file-name "README.org"
				      tq-course-assignments-directory)
      (insert
       (mustache-render (with-temp-buffer (insert-file-contents
					   (expand-file-name
					    "templates/assignments-readme.org"
					    (file-name-directory
					     (locate-library "techela"))))
					  (buffer-string))
			(ht ("host" (techela-course-techela-server tq-current-course))
			    ("tq-root-directory" tq-root-directory))))))
  
  ;; solutions
  (unless (file-directory-p tq-course-solutions-directory)
    (make-directory tq-course-solutions-directory t)
    (with-temp-file (expand-file-name "README.org"
				      tq-course-solutions-directory)
      (insert
       (mustache-render (with-temp-buffer (insert-file-contents
					   (expand-file-name
					    "templates/solutions-readme.org"
					    (file-name-directory
					     (locate-library "techela"))))
					  (buffer-string))
			(ht ("host" (techela-course-techela-server tq-current-course)))))))
  
  ;; student-work
  (unless (file-directory-p tq-course-student-work-directory)
    (make-directory tq-course-student-work-directory t)
    (with-temp-file (expand-file-name "README.org"
				      tq-course-student-work-directory)
      (insert
       (mustache-render (with-temp-buffer (insert-file-contents
					   (expand-file-name
					    "templates/student-work-readme.org"
					    (file-name-directory
					     (locate-library "techela"))))
					  (buffer-string))
			(ht ("host" (techela-course-techela-server tq-current-course)))))))
  
  ;; class-work
  (unless (file-directory-p tq-course-class-work-directory)
    (make-directory tq-course-class-work-directory t)
    (with-temp-file (expand-file-name "README.org"
				      tq-course-class-work-directory)
      (insert
       (mustache-render (with-temp-buffer (insert-file-contents
					   (expand-file-name
					    "templates/class-work-readme.org"
					    (file-name-directory
					     (locate-library "techela"))))
					  (buffer-string))
			(ht ("host" (techela-course-techela-server tq-current-course)))))))

  ;; course
  (unless (file-directory-p tq-course-directory)
    (with-current-directory
     tq-root-directory
     (mygit (format "git clone %s course" (techela-course-course-repo tq-current-course)))))

  ;; Now the gitolite-admin
  (unless (file-directory-p tq-gitolite-admin-dir)
    (with-current-directory tq-root-directory
			    (mygit (format "git clone %s:gitolite-admin"
					   (techela-course-techela-server course)))))

  ;; Setup the new gitolite.conf file
  (with-temp-file
      (expand-file-name
       "conf/gitolite.conf"
       tq-gitolite-admin-dir)
    (insert (mustache-render
	     (with-temp-buffer
	       (insert-file-contents
		(expand-file-name
		 "templates/gitolite.conf"
		 (file-name-directory (locate-library "techela"))))
	       (buffer-string))
	     (ht ("instructor-email" (techela-course-instructor-email course))
		 ("teaching-assistants" (mapconcat (lambda (ta)
						     (car ta))
						   (techela-course-teaching-assistants course)
						   " "))
		 ("techela-server" (techela-course-techela-server course))))))
  
  (with-temp-file
      (expand-file-name
       "conf/students.conf"
       tq-gitolite-admin-dir)
    (insert "@students ="))
  
  (with-current-directory
   tq-gitolite-admin-dir
   (mygit "git commit conf/gitolite.conf -m \"Setup gitolite.conf for techela.\"")
   (mygit "git add conf/students.conf")
   (mygit "git commit conf/students.conf -m \"Add students.conf for techela.\"")
   (mygit "git push origin master")))

(defun tq-setup-course (course)
  "Setup the remote and local directories for COURSE."
  (interactive
   (list
    (prog2
	(tq-check-internet)
	(let ((courses (tq-get-courses)))
	  (cdr (assoc (completing-read
		       "Course name: " courses)
		      courses))))))
  (tq-setup-remote-course course)
  (tq-setup-local-directories course)
  (require 'techela-admin)
  (techela-admin course))

(provide 'techela-setup)

;;; techela-setup.el ends here
