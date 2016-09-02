;;; techela.el --- functions for students to use in techela courses
;; Copyright(C) 2014,2015,2016 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/jmax
;; Version: 0.1.0
;; Keywords: org-mode, education
;; Package-Requires: ((dash "2.11.0") (s "1.10.0") (f "0.18.0") (emacs "24.4"))

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:
(require 'org)
(require 'org-agenda)
(require 'net-utils)
(require 'f)
(require 'techela-git)
(require 'techela-utils)
(require 'techela-setup)
(require 'techela-grade)  		; for gb-set-file-tag
(require 'techela-gradebook)
(require 'easymenu)

;; * Variables
(defvar tq-user-root "~/techela"
  "Root directory to store all techela courses in.")

(defvar tq-user-data-file nil
  "Configuration file for user data.")

(defvar tq-root-directory nil
  "Directory to clone the course and student work to.
This location is not a git repository, but will be the name of the course.")

(defvar tq-course-directory nil
  "Directory where the course content is.
This will be a git repository.  It is inside `tq-root-directory'.")

(defvar tq-assignment-directory nil
  "Directory where the assignment repos will be.
It is inside `tq-root-directory'.")

(defvar tq-current-course nil
  "A techela-course struct of the current course.")

(defvar tq-course-syllabus nil
  "Derived file for the syllabus.")

;; * Getting and opening courses
(defstruct techela-course
  label title course-number
  instructor
  instructor-email
  ;; teaching assistants is a list of '((email "Full Name")
  teaching-assistants
  semester
  year

  ;; a git-repo containing the course material.
  course-repo

  ;; the techela user@server: course@techela.cheme.cmu.edu
  techela-server)


(defun tq-get-courses ()
  "Return a list of (cons description struct) for the available courses.
Available courses are defined in the courses directory. All files
in that directory with no extension are considered courses."
  (mapcar (lambda (f)
	    (with-temp-buffer
	      (insert-file-contents f)
	      (let* ((course-data (read (current-buffer)))
		     (course (apply #'make-techela-course (append '(:label) course-data))))
		(cons (format "%s - %s"
			      (techela-course-label course)
			      (techela-course-title course))
		      course))))
	  (f-files
	   (expand-file-name
	    "courses"
	    (file-name-directory (locate-library "techela")))
	   (lambda (f) (null (f-ext f))))))


(defun tq-get-course-struct (label)
  "Return a course struct for LABEL.
LABEL is a symbol or string and it must a recipe in the courses directory."
  (let ((recipe (expand-file-name
		 (format "courses/%s" label)
		 (file-name-directory (locate-library "techela")))))
    (apply #'make-techela-course (append '(:label) (with-temp-buffer
						     (insert-file-contents recipe)
						     (read (current-buffer)))))))


;; * The main techela function
;;;###autoload
(defun techela (course)
  "Open the techela COURSE.
COURSE is a struct representing the course that should be
interactively selected. If you have not registered for the course,
you will be prompted for setup information."
  (interactive
   (list
    (prog2
	(tq-check-internet)
	(let ((courses (tq-get-courses)))
	  (cdr (assoc (completing-read
		       "Course name: " courses)
		      courses))))))
  
  (setq tq-current-course course
	tq-root-directory (file-name-as-directory
			   (expand-file-name
			    (format "%s" (techela-course-label course))
			    (expand-file-name tq-user-root)))
	tq-course-directory (expand-file-name "course" tq-root-directory)
	tq-course-syllabus (expand-file-name "syllabus.org" tq-course-directory)
	tq-assignment-directory (expand-file-name "assignments" tq-root-directory)
	tq-user-data-file (expand-file-name "techela-user-data" tq-root-directory))

  ;; make directories if needed, including parents
  (unless (file-directory-p tq-root-directory)
    (make-directory tq-root-directory t))
  
  (unless (file-directory-p tq-assignment-directory)
    (make-directory tq-assignment-directory t))

  (unless (file-exists-p tq-user-data-file)
    (tq-register course))

  ;; load directories to variables if they exist
  (let ((data (tq-read-user-data)))
    ;; clone course if we need it. This will be in a repo called "course"
    ;; do not clone if the directory exists.
    (unless (and tq-course-directory (file-exists-p tq-course-directory))
      (let ((default-directory (file-name-as-directory tq-root-directory)))
	(shell-command (format
			"git clone %s course"
			(techela-course-course-repo tq-current-course))))))

  ;; let user know if an update is needed
  (with-current-directory
   tq-course-directory
   (when (> (tq-get-num-incoming-changes) 0)
     (message "%s is out of date. Please wait while I update it" course)
     (tq-update-course)
     (mygit "git checkout origin/master -- syllabus.org")))

  ;; now open syllabus
  (find-file tq-course-syllabus)
  (tq-clean-line-endings)
  (save-buffer)
  (read-only-mode 1)

  (techela-mode)

  ;; load custom setupfile when it exists.
  (when (file-exists-p (expand-file-name
			"lisp/setup.el"
			tq-course-directory))
    (load-file (expand-file-name
		"lisp/setup.el"
		tq-course-directory))))

;;;###autoload
(defun tq-update-course ()
  "Update everything in the course directory."
  (interactive)
  (tq-check-internet)
  (save-some-buffers t) ;;save all buffers
  (with-current-directory
   tq-course-directory
   (mygit "git stash")
   (mygit "git pull origin master")
   (mygit "git submodule update")
   (mygit "git stash pop")
   (mygit "git commit -am \"accepting merge\"")))

;; * Get assignment/turn it in
(defun tq-get-assigned-assignments ()
  "Return a list of assignments from the syllabus.
Assignments are headings that are tagged with :assignment:.  The assignment is
a link in the heading."
  (with-current-buffer (find-file-noselect tq-course-syllabus)
    (org-map-entries
     (lambda ()
       (org-entry-get (point) "CUSTOM_ID"))
     "assignment")))


;;;###autoload
(defun tq-get-assignment (label)
  "Clone the repo corresponding to LABEL and open the directory."
  (interactive
   (list (completing-read "Label: " (tq-get-assigned-assignments))))
  (let ((student-repo-dir (file-name-as-directory
			   (expand-file-name
			    label
			    tq-assignment-directory))))

    ;; Get the assignment by cloning if needed, and rest the remotes.
    (unless (file-directory-p student-repo-dir)
      (make-directory tq-assignment-directory t)
      (let ((default-directory (file-name-as-directory tq-assignment-directory))
	    (repo (format "assignments/%s" label)))
	;; clone and open label.org
	(tq-clone-repo repo)
	;; we need to reset the remotes now
	(with-current-directory
	 student-repo-dir
	 (mygit "git remote rename origin src")
	 (mygit
	  (mustache-render
	   "git remote add origin {{host}}:student-work/{{label}}/{{userid}}-{{label}}"
	   (ht ("host" (techela-course-techela-server tq-current-course))
	       ("label" label)
	       ("userid" (gethash "user-mail-address" (tq-read-user-data)))))))))

    (with-current-directory
     student-repo-dir
     (if (not (string= "" (shell-command-to-string
			   "git status --porcelain")))
	 ;; There are some local changes. We commit them, pull,
	 ;; and commit merges if there are any
	 (progn
	   (message "Local changes found. Please wait while I stash and reapply them.")
	   (mygit "git stash")
	   (mygit "git pull origin master")
	   (mygit "git stash pop"))
       ;; we were clean. Let's pull anyway to get remote changes.
       (message "Checking for remote changes")
       (mygit "git pull origin master"))

     ;; now, open the file
     (find-file (expand-file-name
		 (concat label ".org")
		 student-repo-dir)))))


;;;###autoload
(defun tq-turn-it-in ()
  "Save all buffers, add files, create a SYSTEM-INFO file, commit them and push.
Check *techela log* for error messages."
  (interactive)
  (tq-insert-system-info)

  ;; Let's assume turning in will work, and set the time.
  (gb-set-filetag "TURNED-IN" (current-time-string))

  ;; make sure all buffers are saved
  (save-some-buffers t t)
  
  (mygit "git add *")
  
  (let ((status (car (mygit "git commit -am \"turning in\""))))
    (unless (or (= 0 status)		; no problem
		(= 1 status))		; no change in files
      (gb-set-filetag "TURNED-IN"
		      (concat "Failed: " (current-time-string)))
      (switch-to-buffer "*techela log*")
      (error "Problem committing.  Check the logs")))

  (unless (= 0 (car (mygit "git push -u origin master")))
    (mygit "git commit --amend -m \"*** TURNING IN FAILED ***.\"")
    (gb-set-filetag "TURNED-IN" (concat "Failed: " (current-time-string)))
    (save-buffer)
    (switch-to-buffer "*techela log*")
    (error "Problem pushing to server.  Check the logs"))

  (save-buffer)
  (message
   (let ((choices '("Woohoo, you turned it in!"
		    "Awesome, you rocked that turn in!"
		    "Way to go, you turned it in!"
		    "Great job, you turned it in!"
		    "Sweet, you turned it in!"
		    "Booya, you turned it in!")))
     (nth (cl-random (length choices)) choices))))

;;;###autoload
(defun tq-update-my-assignments ()
  "Open each assignment.
This will pull each assigned assignment."
  (loop for assignment in (tq-get-assigned-assignments)
	do
	(org-open-link-from-string (format "[[assignment:%s]]" assignment))))


;; * Course agenda

;;;###autoload
(defun tq-agenda ()
  "Show the course agenda from the syllabus."
  (interactive)
  (let ((org-agenda-custom-commands '(("c" "Course Agenda"
				       ((tags-todo "+DEADLINE>=\"<today>\""
						   ((org-agenda-overriding-header
						     "Press q to quit\nUpcoming Deadlines")))
					;; now the agenda
					(agenda ""
						((org-agenda-overriding-header "two week agenda")
						 (org-agenda-ndays 14)
						 (org-agenda-tags-todo-honor-ignore-options t)
						 (org-agenda-todo-ignore-scheduled nil)
						 (org-agenda-todo-ignore-deadlines nil)
						 (org-deadline-warning-days 0)
						 ))

					;; and last a global todo list
					(todo "TODO")))))
	(org-agenda-files (list tq-course-syllabus)))
    (org-agenda "" "c")))


;; * links

;; This downloads the assignment repo for the student to work in
(org-add-link-type
 "assignment"
 (lambda (arg)
   (tq-check-internet)
   (tq-get-assignment arg)))


;; download the solution
(org-add-link-type
 "solution"
 (lambda (label)
   (tq-check-internet)
   (with-current-directory
    tq-root-directory
    (unless (file-exists-p "solutions")
      (make-directory "solutions"))
    (with-current-directory
     "solutions"
     (if (file-exists-p label)
	 ;; we have the solution
	 (progn
	   (find-file (concat label "/" label ".org"))
	   ;; update just for good measure
	   (tq-update))
       ;; no file
       (mygit (format "git clone %s:solutions/%s"
		      (techela-course-git-server tq-current-course)
		      label))
       (find-file (concat label "/" label ".org")))))))


;; these will usually be in class or optional exercises. This is a
;; link for clarity of intention for students.
(org-add-link-type
 "exercise"
 (lambda (arg)
   (tq-check-internet)
   (tq-get-assignment arg)))


;; multiple choice link.
(org-add-link-type
 "mc"
 (lambda (link)
   (org-entry-put (point) "ANSWER" link)
   (save-restriction
     (save-excursion
       (org-narrow-to-subtree)
       (goto-char (point-max))
       (unless (bolp)
	 (insert "\n"))
       (gb-set-filetag (org-entry-get (point) "ID") link)))))


;; Link to record answers. ans:label::data
;; save in userid-label-data.dat
(org-add-link-type
 "ans"
 (lambda (path)
   (let* ((fields (split-string path "::"))
	  (label (nth 0 fields))
	  (data (nth 1 fields))
	  (data-file (format "%s-%s.dat"
			     (gethash "user-mail-address" (tq-read-user-data))
			     label)))
     (with-temp-file data-file
       (insert data))
     (mygit (format "git add %s" data-file))
     (mygit (format "git commit -m \"%s\"" data-file))
     (mygit "git push origin master"))))


;; * email functions
;;;###autoload
(defun tq-submit-by-email ()
  "Submit contents of current directory as a zip file attached to an email.
This is normally only done after the deadline, when you cannot
push to the git repo, or when there is some issue with the git
server. There must be extenuating circumstances for this to be
used."
  (interactive)
  (tq-check-internet)

  (unless (executable-find "zip")
    (error "Could not find a zip executable"))

  (let ((zip-name (concat
		   (gethash "user-mail-address" (tq-read-user-data))
		   "-"
		   (file-name-sans-extension
		    (file-name-nondirectory
		     (buffer-file-name))))))
    ;; update system file
    (tq-insert-system-info)
    (gb-set-filetag "TURNED-IN-BY-EMAIL:" (current-time-string))
    (save-some-buffers t)

    ;; remove zip if it exists.
    (when (file-exists-p (concat zip-name ".zip"))
      (delete-file (concat zip-name ".zip")))

    ;; add everything in this directory to get it clean, except for the zip file.
    (mygit "git add *")

    (let ((status (car (mygit "git commit -am \"saving for email submit\""))))
      (unless (or (= 0 status)		; no problem
		  (= 1 status))		; no change in files
	(switch-to-buffer "*techela log*")
	(error "Problem committing.  Check the logs")))

    (mygit "git tag -f -a turned_in -m \"Tagging version turned in by email.\"")

    ;; make zip of directory
    (shell-command (format "zip -v -r %s .git *"
			   zip-name))

    ;; the .git folder is locally not in sync with the one turned in.
    (message-mail)
    (mml-attach-file (concat zip-name ".zip"))
    (message-goto-to)
    (insert "jkitchin@andrew.cmu.edu")
    (message-goto-subject)
    (insert (format "[%s email turnin]" (techela-course-label tq-current-course)))
    ;; TODO: still cmu specific
    (let ((send-mail-function 'smtpmail-send-it)
	  (user-mail-address (gethash "user-mail-address" (tq-read-user-data)))
	  (smtpmail-smtp-server "smtp.andrew.cmu.edu")
	  (smtpmail-smtp-service 587)
	  (smtpmail-stream-type nil)
	  (smtpmail-starttls-credentials '(("smtp.andrew.cmu.edu" 587 nil nil)))
	  (starttls-use-gnutls t)
	  (starttls-gnutls-program "gnutls-cli")
	  (mail-host-address "andrew.cmu.edu"))
      (message-send-and-exit))))


;;;###autoload
(defun tq-email ()
  "Construct and send an email to the instructor."
  (interactive)
  (let ((email-body
	 (format "Type your note below here, and press C-c C-c when you are done to send it:


======================================================
file: %s
line %s: %s
repo remote origin: %s
======================================================"
		 (buffer-file-name)
		 (what-line)
		 (thing-at-point 'line)
		 (nth 1 (mygit "git config --get remote.origin.url")))))

    (compose-mail-other-frame)
    (message-goto-to)
    (insert (techela-course-instructor-email tq-current-course))
    (message-goto-subject)
    (insert (format "[%s] email" (symbol-name (techela-course-label tq-current-course))))
    (message-goto-body)
    (insert email-body)
    (message-goto-body) ; go back to beginning of email body
    (next-line 2)         ; and down two lines
    (message "Type C-c C-c to send message")))

;;;###autoload
(defun tq-send-error-report ()
  "Send an error report to the instructor."
  (interactive)
  (compose-mail-other-frame)
  (message-goto-to)
  (insert (techela-course-instructor-email tq-current-course))
  (message-goto-subject)
  (insert (format "[%s] debug report" (symbol-name (techela-course-label tq-current-course))))
  (message-goto-body)
  (insert "Tell me what you were doing. Then press C-c C-c to send the message.


Messages\n==========\n")
  (when (get-buffer "*mygit-process*")
    (insert (with-current-buffer "*mygit-process*" (buffer-string)))
    (insert "\n"))
  (when (get-buffer "*techela log*")
    (insert (with-current-buffer "*techela log*" (buffer-string)))
    (insert "\n"))
  (message-goto-body) ; go back to beginning of email body

  (next-line 2)         ; and down two lines

  (message "Type C-c C-c to send message"))

;; * Grade report

;;;###autoload
(defun tq-grade-report ()
  "Generate a *grade report* buffer with a summary of the graded assignments."
  (interactive)
  (switch-to-buffer "*grade report*")
  (erase-buffer)
  (org-mode)
  (insert "#+TITLE: Grade report

|label |   score  |             points  |       category|
|--------------------------------------------------------
")
  (dolist (label (tq-get-assigned-assignments))
    (let ((grade)
	  (points)
	  (category)
	  (fname))
      (with-current-buffer (find-file-noselect
			    tq-course-syllabus)
	(save-restriction
	  (widen)
	  (beginning-of-buffer)
	  ;; This link relies on a CUSTOM_ID
	  (org-open-link-from-string (format "[[#%s]]" label))
	  (setq points (org-entry-get (point) "POINTS"))
	  (setq category (org-entry-get (point) "CATEGORY"))))

      ;; check if we need to update
      (if (file-exists-p (expand-file-name label tq-root-directory))
	  (progn
	    (with-current-directory
	     (expand-file-name label tq-root-directory)
	     (when (> (tq-get-num-incoming-changes) 0)
	       ;; make us clean first
	       (save-some-buffers t) ; save everything
	       (mygit "git add *") ; add anything new
	       (mygit "git commit -am \"my changes\"")
	       ;; then pull
	       (mygit "git pull origin master")
	       ;; accept conflicts if there are any
	       (mygit "git commit -am \"accepting merge\""))
	     )

	    ;; The student assignment will be in root/label/label.org
	    (setq fname (expand-file-name (concat label "/" label ".org") tq-root-directory))

	    (when (file-exists-p fname)
	      (setq grade (gb-get-grade fname)))

	    (insert (format "|[[%s][%s]]|  %10s|%20s|%20s|\n" fname label grade points category)))
	;; no dir found
	(insert (format "|%s|not found|%20s|%20s|\n" label points category)))))
  (previous-line)
  (org-ctrl-c-ctrl-c)
  (goto-char (point-min))
  (switch-to-buffer "*grade report*"))

;; * menu and minor mode
;;;###autoload
(defun tq-open-syllabus ()
  "Open the course syllabus."
  (interactive)
  (find-file tq-course-syllabus)
  (read-only-mode 1))

(defvar techela-mode-map
  (let ((map (make-sparse-keymap)))
    ;; no key-bindings for now.
    map)
  "Keymap for function `techela-mode'.")


(easy-menu-define techela-menu techela-mode-map "Techela menu"
  '("techela"
    ["Open syllabus" tq-open-syllabus t]
    ["Turn assignment in" tq-turn-it-in t]
    ["Assignment history" tq-git-log t]
    ["Search course files" tq-search t]
    ["Table of contents" tq-toc t]
    ["Course index" tq-index t]
    ["Course agenda" tq-agenda t]
    ["Get grade report" tq-grade-report t]
    ["Email instructor" tq-email t]
    ["Update current file" tq-update t]
    ["Send error report" tq-send-error-report t]
    ["Quit techela" tq-quit t]))


(define-minor-mode techela-mode
  "Minor mode for techela

\\{techela-mode-map}"
  :lighter " techela"
  :global t
  :keymap techela-mode-map)

;;;###autoload
(defun tq-quit ()
  "Quit techela."
  (interactive)
  (techela-mode -1))

(provide 'techela)

;;; techela.el ends here
