;;; techela-setup.el --- setup techela courses 

;;; Commentary:
;; Here we create necessary scripts to run ssh and git, and collect user
;; information for a course. We generate the ssh keys and send them to the
;; instructor.

(require 'json)
(require 'mustache)
(require 'ht)


;;; Code:

(defun tq-config-read-data ()
  "Read and return the data in `tq-config-file'."
  (if (file-exists-p tq-user-config-file)
      (let ((json-object-type 'hash-table))
	(json-read-file tq-user-config-file))
    ;; no file exists, return an empty hash
    (make-hash-table :test 'equal)))


(defun tq-config-write-data (data)
  "Write DATA to `tq-config-file'.
DATA should be obtained and modified from `tq-config-read-data'."
  (with-temp-file tq-user-config-file
    (insert (json-encode-hash-table data))))


(defun tq-setup-user ()
  "Makes sure these variables are set:  `user-full-name',
`user-mail-address'. Configure git with this information where
needed."

  ;; Full name
  (let ((data (tq-config-read-data)))
    (unless (gethash "user-full-name" data)
      (puthash "user-full-name" (read-from-minibuffer "Enter your full name: ") data)
      (tq-config-write-data data)))

  ;; email address
  (let ((data (tq-config-read-data)))
    (unless (gethash "user-mail-address" data)
      (puthash "user-mail-address" (read-from-minibuffer "Enter your email address: ") data)
      (tq-config-write-data data)))

  (unless (executable-find "git")
    (error "I cannot find git.  You cannot use techela"))

  (when (string= "" (shell-command-to-string "git config --global user.name"))
    (shell-command (format "git config --global user.name \"%s\""
			   (gethash "user-full-name" data))))

  (when (string= "" (shell-command-to-string "git config --global user.email"))
    (shell-command (format "git config --global user.email %s"
			   (gethash "user-mail-address" data))))

  (when (string= "" (shell-command-to-string "git config --global push.default"))
    (shell-command "git config --global push.default matching"))
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

  ;; now we know the tq-root-directory directory exists, check
  ;; for userid and userid.pub, and make a pair if needed

  ;; here we look for the existence of tq-root-directory/userid.pub which is a
  ;; private ssh key. We do not replace these since they are registered.
  (unless (file-exists-p (expand-file-name
			  (concat
			   (gethash "user-mail-address" (tq-config-read-data))
			   ".pub")
			  tq-root-directory))
    ;; we make one with no password
    (shell-command (format "ssh-keygen -t rsa -f \"%s\" -N \"\""
			   (expand-file-name
			    (gethash "user-mail-address" (tq-config-read-data))
			    tq-root-directory))))

  ;; Now we add this to the config file. 
  (with-temp-file (expand-file-name
		   "techela-config"
		   tq-root-directory)
    (insert (mustache-render
	     "Host {{host}}
  User {{course}}
  IdentityFile {{identify-file}}
  StrictHostKeyChecking no
  UserKnownHostsFile /dev/null\n"
	     (ht ("host" (nth 1 (split-string (techela-course-techela-server tq-current-course) "@")))
		 ("course" (symbol-name (techela-course-label  tq-current-course)))
		 ("identify-file" (expand-file-name
				   (gethash "user-mail-address" (tq-config-read-data))
				   tq-root-directory))))))

  (with-temp-file (expand-file-name
		   "techela_ssh"
		   tq-root-directory)
    (insert
     (format "#!/bin/bash
# custom ssh for running git in batch mode for techela with the user-key
exec ssh -F \"%s\" -o \"BatchMode yes\" \"$@\"
# end" (expand-file-name "techela-config" tq-root-directory))))

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

  ;; Finally setup and send the email
  ;; now create an email and send the key to the instructor
  (compose-mail)
  (message-goto-from)
  (kill-line)
  (insert (gethash "user-mail-address" (tq-config-read-data)))
  (message-goto-to)
  (insert (techela-course-instructor-email tq-current-course))
  (message-goto-subject)
  (insert (format "[%s] %s pubkey" (techela-course-label tq-current-course)
		  (gethash "user-mail-address" (tq-config-read-data))))
  (mml-attach-file (expand-file-name
		    (concat (gethash "user-mail-address"
				     (tq-config-read-data)) ".pub")
		    tq-root-directory))
  (message-goto-body)
  ;; let us get some user/computer information
  (tq-insert-system-info)
  (insert (with-temp-buffer
	    (insert-file-contents "SYSTEM-INFO")
	    (buffer-string)))
  (delete-file "SYSTEM-INFO")
  (let ((send-mail-function 'smtpmail-send-it)
	(user-mail-address (gethash "user-mail-address" (tq-config-read-data)))
	(smtpmail-smtp-server "relay.andrew.cmu.edu")
	(mail-host-address "andrew.cmu.edu"))
    (message-send-and-exit)))

(defun tq-register (course)
  "Register for COURSE.
COURSE is a struct representing the course. This sets up your
local machine and emails the instructor your ssh pub key. You
cannot access the course until you get an email confirmation
back."
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
	tq-user-config-file (expand-file-name "techela-user-config" tq-root-directory))

  ;; make root directory if needed, including parents
  (unless (file-exists-p tq-root-directory)
    (make-directory tq-root-directory t))
  
  (tq-setup-user)
  (tq-setup-ssh)
  (message "Please wait for further instructions."))


(provide 'techela-setup)

;;; techela-setup.el ends here
