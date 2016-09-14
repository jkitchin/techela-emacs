;;; techela-admin.el --- Techela Administration functions
;;

;;; Commentary:
;; Techela is a Technology enhanced learning and assessment
;; environment.  A techela course is hosted on a gitolite server. 
;; See README.org and FAQ.org
;;

(require 'cl-lib)
(require 'techela)
(require 'techela-roster)
(require 'techela-grade)
(require 'techela-gradebook)
(require 'techela-git)
(require 'mustache)
(require 'ht)
(require 'f) ; file utilities

;;; Code:

(defvar tq-gitolite-admin-dir nil
  "Derived variable that stores the location of the admin directory.")

(defvar tq-course-student-work-dir nil
  "Derived variable to the location of student work.")

;; * The main techela-admin command
;;;###autoload
(defun techela-admin (course)
  "Open the course dashboard for COURSE.
COURSE is a struct representing the course that should be
interactively selected."
  (interactive
   (list
    (prog2
	(tq-check-internet)
	(let ((courses (tq-get-courses)))
	  (cdr (assoc (completing-read
		       "Course name: " courses)
		      courses))))))

  ;; First load things like techela
  (techela course)

  ;; we should have a tq-root-directory now, so we set all the derived variables
  (setq tq-gitolite-admin-dir (file-name-as-directory
			       (expand-file-name
				"gitolite-admin"
				tq-root-directory))

	;; path to the roster file
	tq-roster-file (expand-file-name
			"roster.org"
			tq-gitolite-admin-dir)

	;; local directory where assignment folders are
	tq-course-assignments-dir (file-name-as-directory
				   (expand-file-name
				    "assignments"
				    tq-root-directory))

	tq-course-solutions-dir (file-name-as-directory
				 (expand-file-name
				  "solutions"
				  tq-root-directory))
	;; public facing course
	tq-course-directory (file-name-as-directory
			     (expand-file-name
			      "course"
			      tq-root-directory))

	;; local place where userid folders can be found
	tq-course-student-work-dir (file-name-as-directory
				    (expand-file-name
				     "student-work"
				     tq-root-directory))

	;; local place where class exercise folders can be found
	tq-course-class-work-dir (file-name-as-directory
				  (expand-file-name
				   "class-work"
				   tq-root-directory)))

  ;; Check that we have everything in place
  (unless (file-exists-p tq-course-solutions-dir)
    (make-directory tq-course-solutions-dir t))

  (unless (file-exists-p tq-course-assignments-dir)
    (make-directory tq-course-assignments-dir t))

  ;; where common class repos will be
  (unless (file-exists-p tq-course-class-work-dir)
    (make-directory tq-course-class-work-dir t))

  ;; where individual student repos will be
  (unless (file-exists-p tq-course-student-work-dir)
    (make-directory tq-course-student-work-dir))

  ;; make sure we have the gitolite admin folder
  (unless (file-exists-p tq-gitolite-admin-dir)
    (with-current-directory
     tq-root-directory
     (mygit (format "git clone %s:gitolite-admin"
		    (techela-course-techela-server tq-current-course)))))

  ;; Make sure we have the course folder
  (unless (file-exists-p tq-course-directory)
    (with-current-directory
     tq-root-directory
     (mygit (format "git clone %s"
		    (techela-course-course-repo tq-current-course)))))
  
  ;; load custom setupfile when it exists.
  (when (file-exists-p (expand-file-name
			"lisp/setup.el"
			tq-course-directory))
    (load-file (expand-file-name
		"lisp/setup.el"
		tq-course-directory)))

  ;; open with the status view
  (tq-status))


;; * REPO creation functions

(defun tq-get-repo-name (label userid)
  "Return student private reponame on server for assignment LABEL of USERID.

For example: student-work/userid/userid-label."
  (format "student-work/%s/%s-%s" label userid label))


(defun tq-create-edit-repo-conf (reponame &optional R RW RW+)
  "Create a repo file for REPONAME with permissions. REPONAME may be a path.
R is a list of users with Read permission
RW is a list of users with Read/Write permission
RW+ is a list of users with force permission.

This creates a repo.conf file in the admin conf directory, then
commits it.  No push is done to create the repo on the server.
This is intentional, as sometimes you may want to create several
conf files before pushing. See `tq-create-edit-repo' which will
create and push the repo.

An existing conf file is overwritten, which allows you to change
permissions on an existing repo."
  (let ((repo-conf-dir (file-name-as-directory
			(expand-file-name "conf" tq-gitolite-admin-dir))))
    (with-current-directory
     repo-conf-dir
     (let ((repo-file
	    (expand-file-name
	     (concat reponame ".conf")
	     repo-conf-dir)))
       ;; make sure we have the folder to put it in. This mirrors what
       ;; is on gitolite
       (unless (file-exists-p (file-name-directory repo-file))
	 (make-directory (file-name-directory repo-file) t))

       (with-temp-file repo-file
	 (insert
	  (concat "repo " reponame "\n"
		  (when R
		    (format "    R = %s\n" (mapconcat 'identity R " ")))
		  (when RW
		    (format "    RW = %s\n" (mapconcat 'identity RW " ")))
		  (when RW+
		    (format "    RW+ = %s\n" (mapconcat 'identity RW+ " ")))
		  "\n")))

       (tq-log "-------------------- tq-create-edit-repo ---------------------\n")

       (mygit (format "git add %s" (file-relative-name repo-file repo-conf-dir)))

       (mygit (format "git commit %s -m \"create/edit %s\""
		      (file-relative-name repo-file repo-conf-dir)
		      reponame))
       (tq-log "--------------------------------------------------------------\n")))))


(defun tq-create-edit-repo (reponame &optional R RW RW+)
  "This creates the repo.conf file for REPONAME and pushes it.
Optional argument R read-only permission.
Optional argument RW read-write permission."
  (tq-create-edit-repo-conf reponame R RW RW+)
  (let ((repo-conf-dir (file-name-as-directory
			(expand-file-name "conf" tq-gitolite-admin-dir))))
    (with-current-directory
     repo-conf-dir
     (mygit "git push"))))

;; * Assign/collect/return functions
;; ** Individual assignments
(defun tq-assign-to (label userid)
  "Assign assignment LABEL to USERID.
This gives the student RW access to a repo they can push to. You
need to make sure the student is in the roster, and that
`tq-roster-update' has been run so the student also has R access
to assignments."
  (interactive
   (list
    (completing-read
     "Label: "
     (tq-get-possible-assignments) nil t)
    (completing-read "Userid: " (tq-get-userids))))
  ;; first, create the repo and push it.
  (let* ((repo-name (tq-get-repo-name label userid)))
    ;; this pushes so the effect is immediate
    (tq-create-edit-repo repo-name
			 nil        ;;R permission
			 (list userid))))  ;; RW permission


(defun tq-collect-from (label userid)
  "Collect assignment LABEL from USERID.
This sets that repo to R access for USERID. We do not pull the assignment here."
  (interactive
   (list
    (completing-read "Label: " (tq-get-assigned-assignments) nil t)
    (completing-read "Userid: " (tq-get-userids))))
  (let* ((repo-name (tq-get-repo-name label userid)))
    ;; this pushes so the effect is immediate
    (tq-create-edit-repo repo-name
			 (list userid)))) ;; R permission


(defun tq-return-to (label userid)
  "Commit changes to LABEL and push to USERID repo."
  (interactive
   (list
    (completing-read "Label: " (tq-get-assigned-assignments) nil t)
    (completing-read "Userid: " (tq-get-userids))))
  (let* ((repo-name (tq-get-repo-name label userid))
	 (repo-dir-name (expand-file-name
			 repo-name
			 tq-root-directory))
	 (repo-dir (file-name-as-directory
		    (expand-file-name
		     repo-name
		     tq-root-directory))))
    (with-current-directory
     repo-dir
     (let ((process-environment (cons *GIT_SSH* process-environment)))
       (start-process-shell-command
	"tq-return-to"
	"*ta return to*"
	"git add * && git commit -am \"Returning\" && git push")))))

;; ** class assignments
;;;###autoload
(defun tq-update-all-student-work ()
  "Loop through all assignment repos and pull/clone them locally.

We do not check if our local copy is up to date first.  we probably should."
  (interactive)
  (dolist (label (tq-get-assigned-assignments))
    (tq-pull-repos label)))

;; * Create assignment/solution
;; ** assignment
(defun tq-get-possible-assignments ()
  "Return list of assignments in the assignments directory."
  (with-current-directory
   tq-course-assignments-dir
   (remove-if-not
    'file-directory-p
    ;; list directories, except for . and ..
    (directory-files tq-course-assignments-dir nil "[^.{1,2}]"))))

;;;###autoload
(defun tq-create-assignment (label)
  "Create or edit an assignment LABEL.
Interactively prompt for points, category, rubric and due date."
  (interactive (list
		(completing-read "Label: " (tq-get-possible-assignments))))

  (let* ((assignment-dir (file-name-as-directory
			  (expand-file-name
			   label
			   tq-course-assignments-dir)))
	 (assignment-org (expand-file-name
			  (concat label ".org")
			  assignment-dir)))
    (if (file-exists-p assignment-org)
	(find-file assignment-org)
      
      ;; no file found. make one.
      (with-current-directory
       tq-course-assignments-dir
       (mygit (format "git clone %s:assignments/%s"
		      (techela-course-techela-server tq-current-course)
		      label)))

      (with-temp-file (expand-file-name
		       ".gitignore"
		       assignment-dir)
	(insert "\#*\n*ltxpng*\n"))

      ;; create the org file and make sure it has the right filetags.
      (find-file (expand-file-name
		  (concat label ".org")
		  assignment-dir))
      (gb-set-filetag "ASSIGNMENT" label)
      
      (unless (gb-get-filetag "POINTS")
	(gb-set-filetag "POINTS" (read-input "Points: ")))

      (unless (gb-get-filetag "CATEGORY")
	(gb-set-filetag "CATEGORY" (completing-read "Category: " (tq-get-categories))))

      (unless (gb-get-filetag "RUBRIC")
	(gb-set-filetag "RUBRIC" (cdr (assoc (completing-read "Rubric: "
							      (mapcar 'car tq-rubrics))
					     tq-rubrics))))
      (unless (gb-get-filetag "DUEDATE")
	(gb-set-filetag "DUEDATE" (with-temp-buffer
				    (org-time-stamp '())
				    (buffer-string))))
      (goto-char (point-max))
      (insert "\n\n[[elisp:tq-turn-it-in][Turn it in]]\n"))))


(defun tq-create-assignment-repos (label)
  "Create repos for all students in the roster for an assignment LABEL.

1. This will prompt you for a LABEL, which is a directory in the
assignment dir.

2. Create empty repo confs for all students in the course, with
instructor only access, and push that to create the repos on the
server. Students cannot access these repos yet. That is where
they will push their work.

We do not clone these yet, because they are empty.

You must \"assign\" the assignment in another step, which
involves giving the students read/write permissions. See
`tq-assign-assignment'." 
  (let* ((userids (tq-get-userids))
	 (repos (mapcar (lambda (userid)
			  (tq-get-repo-name label userid))
			userids)))

    ;; create all the conf files
    (mapcar
     (lambda (reponame) (tq-create-edit-repo-conf reponame nil '("@instructors")))
     repos)

    ;; push them all at once.
    (with-current-directory
     tq-gitolite-admin-dir
     (mygit "git push origin master"))))

;;;###autoload
(defun tq-assign-assignment (label)
  "Assign the assignment with LABEL to students.

1. This will prompt you for a LABEL.

2. Set repos to RW for students.

3. Update the syllabus with the assignment.

The assignment must have POINTS, CATEGORY, RUBRIC and DUEDATE
defined.  The syllabus must be in the right place:
course/syllabus.org, and it must have a categories table, and an
assignments section. This function updates the assignments
section.
"
  (interactive (list
		(completing-read
		 "Label: "
		 (tq-get-possible-assignments) nil t)))

  ;; First we update the syllabus. Let us get the details from the assignment file
  (let (POINTS CATEGORY DUEDATE RUBRIC)
    (with-current-buffer (find-file-noselect
			  (expand-file-name
			   (format "%s.org" label) ;; the org-file
			   (expand-file-name label tq-course-assignments-dir)))

      (setq POINTS (gb-get-filetag "POINTS")
	    CATEGORY (gb-get-filetag "CATEGORY")
	    RUBRIC (gb-get-filetag "RUBRIC")
	    DUEDATE (gb-get-filetag "DUEDATE"))

      (unless (and POINTS CATEGORY RUBRIC DUEDATE)
	(error "You must define the points, category, duedate and rubric in the assignment file"))

      ;; make sure everything is committed and pushed
      (with-current-directory
       (expand-file-name label tq-course-assignments-dir)
       (mygit "git add *")
       (mygit "git commit -m \"final commit before assignment.\"")
       (mygit "git push origin master"))

      ;; Get syllabus
      (with-current-buffer (find-file-noselect
			    (expand-file-name "syllabus.org"
					      tq-course-directory))
	(save-restriction
	  (widen)
	  (beginning-of-buffer)
	  ;; This link relies on a CUSTOM_ID
	  (org-open-link-from-string "[[#assignments]]")
	  (org-narrow-to-subtree)
	  ;; we add an assignment headline, as long as here is not one already
	  (let ((entries
		 (org-map-entries
		  (lambda ()
		    (nth 4 (org-heading-components))))))
	    (unless (-contains? entries (format "assignment:%s" label))
	      ;; add new entry
	      (goto-char (point-max))
	      (toggle-read-only -1)
	      (insert "\n** TODO assignment:" label)
	      (org-set-tags-to ":assignment:")
	      (goto-char (point-max))
	      (org-entry-put (point) "CATEGORY" CATEGORY)
	      (org-entry-put (point) "POINTS" POINTS)
	      (org-entry-put (point) "CUSTOM_ID" label)
	      (org-entry-put (point) "RUBRIC" RUBRIC)
	      (org-deadline nil DUEDATE)
	      (goto-char (point-max))
	      (insert "\n")
	      (toggle-read-only 1)
	      (save-buffer)))))
      
      ;; Finally, we need to commit the syllabus change, and push it.
      (with-current-directory
       tq-course-directory
       (mygit (format "git commit syllabus.org -m \"added assignment %s\"" label))
       (mygit "git push"))
      
      ;; update repo permissions
      (mapcar
       (lambda (userid)
	 (tq-create-edit-repo-conf
	  (tq-get-repo-name label userid)
	  nil		  ;; R
	  (list userid))) ;; RW
       (tq-get-userids))

      ;; push them all at once.
      (with-current-directory
       tq-gitolite-admin-dir
       (mygit "git push origin master"))

      ;; Now, give them read access on the assignment. The assignment
      ;; is created as a wild repo, so we do permissions different on
      ;; these than on other types of repos.
      (shell-command (format "ssh %s perms assignments/%s + READERS @students"
			     (techela-course-techela-server tq-current-course) label)))))


(defun tq-collect(label)
  "Collect LABEL from students

1. This will prompt you for a LABEL, which is a directory in the
assignments dir.
2. Set student permission to R in each repo.conf.
3. Push the new conf files to the server.

This does not pull the repos. See `tq-pull-repos'."
  (interactive (list
		(completing-read "Label: " (tq-get-assigned-assignments) nil t)))

  (dolist (userid (tq-get-userids))
    (tq-create-edit-repo-conf
     (tq-get-repo-name label userid) (list userid) nil))         

  ;; push the repo permission changes all at once.
  (with-current-directory
   tq-gitolite-admin-dir
   (mygit "git push origin master"))

  ;; change state of assignment in syllabus
  (set-buffer (find-file-noselect tq-course-syllabus)) 
  (toggle-read-only -1)
  (org-open-link-from-string (format "[[#%s]]" label))
  (org-todo "COLLECTED")
  (save-buffer) 

  ;; push change
  (with-current-directory
   tq-course-directory
   (mygit (format "git commit syllabus.org -m \"collected %s\"" label))
   (mygit "git push origin master")))


(defun tq-pull-repos (label)
  "Pull the assignment LABEL repo for each student.  This is not
a fast operation because it requires a pull for every
student. You may want to run `tq-collect' to change the
permissions of the repo to Read-only first."
  (interactive (list
		(completing-read "Label: " (tq-get-assigned-assignments) nil t)))

  (dolist (userid (tq-get-userids))
    (let* ((repo-name (tq-get-repo-name label userid))
	   (repo-dir-name (expand-file-name
			   repo-name
			   tq-root-directory))
	   (repo-dir (file-name-as-directory
		      (expand-file-name
		       repo-name
		       tq-root-directory))))

      ;; make sure path to repo exists
      (unless (file-exists-p repo-dir-name)
	(make-directory  (file-name-directory repo-dir-name) t))

      (if (file-exists-p repo-dir)
	  ;; we have a copy fo the work so we pull it.
	  (with-current-directory
	   repo-dir
	   (mygit "git pull"))

	;; repo-dir did not exist. So we clone it.
	(with-current-directory
	 (file-name-directory repo-dir-name)
	 (mygit (format "git clone %s:%s"
			(techela-course-techela-server tq-current-course)
			repo-name)))))
    (message "pulled %s" userid)))


(defun tq-pull-all-repos ()
  "Pull all repos of assigned assignments."
  (interactive)
  (loop for assignment in (tq-get-assigned-assignments)
	do (tq-pull-repos assignment)))

(defun tq-return (label)
  "Return assignment LABEL for each student.
This means go into each repo, commit all changes, and push them."
  (interactive (list
		(completing-read
		 "Label: "
		 (tq-get-assigned-assignments)
		 nil t)))
  (dolist (userid (tq-get-userids))
    (let* ((repo-name (tq-get-repo-name label userid))
	   (repo-dir-name (expand-file-name
			   repo-name
			   tq-root-directory))
	   (repo-dir (file-name-as-directory
		      (expand-file-name
		       repo-name
		       tq-root-directory))))
      (when
	  (file-exists-p repo-dir)
	(with-current-directory
	 repo-dir
	 ;; only push if changes detected
	 (if (or (> (tq-git-n-untracked-files) 0) ; we have untracked files
		 (> (tq-git-n-modified-files) 0)  ; we have modified files
		 (> (nth 0 (tq-git-n-commits)) 0)) ; local is ahead of remote
	     (progn
	       (message "returning %s" userid)
	       (mygit "git add *")
	       (mygit (format  "git commit -am \"Returning %s to %s.\""
			       label userid))
	       (mygit "git push origin master")
	       (message "returned %s" userid))
	   (message "no changes detected for %s" userid))))))

  ;; change state of assignment in syllabus
  (set-buffer (find-file-noselect
	       tq-course-syllabus))
  (org-open-link-from-string (format "[[#%s]]" label))
  (org-todo "GRADED")
  (save-buffer)

  ;; push change to syllabus
  (with-current-directory
   tq-course-directory
   (mygit (format  "git commit syllabus.org -m \"Returned %s\"" label))
   (mygit "git push origin master")))


(defun tq-open-assignment (label userid)
  "Open the USERID assignment LABEL.
This will be in student-work/label/userid-label/userid-label.org."
  (interactive (list (completing-read "Label: " (tq-get-assigned-assignments))
		     (completing-read "Userid: " (tq-get-userids))))
  (let* ((repo (tq-get-repo-name label userid))
	 (repo-dir (file-name-as-directory
		    (expand-file-name
		     repo
		     tq-root-directory))))
    (message "looking for %s %s"  repo repo-dir)
    (if (file-exists-p repo-dir)
	(with-current-directory
	 repo-dir
	 ;; initially there may not be tracking information, so we are specific in the pull
	 (mygit "git pull origin master")
	 (find-file (concat label ".org"))
	 (grade-mode))
      ;; else. no dir. make the student dir if needed, and clone the repo
      (let ((label-dir (expand-file-name
			label
			tq-course-student-work-dir)))
	(unless (file-exists-p label-dir)
	  (make-directory label-dir t))

	(with-current-directory
	 label-dir
	 (mygit (format "git clone %s:%s"
			(techela-course-techela-server tq-current-course) repo))))
      ;; now open it.
      (with-current-directory
       repo-dir
       (find-file (concat label ".org"))
       (grade-mode)
       (buffer-name)))))

;; ** Solutions

(defun tq-create-solution (label)
  "Create or edit a solution LABEL.
This creates the repo if needed, and copies the assignment to the
solution directory. It does not give students access to the
solution. It also does not commit or push your work for you. You
need to run `tq-release-solution' to give students access to the
solution."
  (interactive (list
		(completing-read
		 "Label: "
		 (tq-get-possible-assignments) nil t)))

  (let ((solution-dir (file-name-as-directory
		       (expand-file-name
			label
			tq-course-solutions-dir))))
    (unless (file-exists-p solution-dir)
      ;; no dir found. create the repo and directory. It is a wild
      ;; repo on gitolite.
      (with-current-directory
       tq-course-solutions-dir
       (mygit (format "git clone %s:solutions/%s"
		      (techela-course-techela-server tq-current-course)
		      label))

       ;; now, copy assignment org in as basis for solution unless it now exists.
       (let ((assign-org (expand-file-name
			  (concat label ".org")
			  (expand-file-name
			   label
			   tq-course-assignments-dir)))
	     (soln-org (expand-file-name
			(concat label ".org")
			solution-dir))) 
	 ;; If we don't have the solution org-file, we copy everything over from
	 ;; the assignment repo, except the .git repo.
	 (unless (file-exists-p soln-org) 
	   (with-current-directory
	    solution-dir
	    (loop for f in 
		  (f-entries
		   (expand-file-name
		    label
		    tq-course-assignments-dir)
		   (lambda (f) (not (s-ends-with-p ".git" f))))
		  do
		  (if (file-directory-p f)
		      (copy-directory f solution-dir nil t t)
		    (copy-file f solution-dir t))))))))
    
    ;; open the file
    (find-file (expand-file-name
		(concat label ".org")
		solution-dir))))


(defun tq-release-solution (label)
  "Give students read access to the solution LABEL.
See also `tq-close-solution'."
  (interactive (list
		(completing-read
		 "Label: "
		 (tq-get-possible-assignments) nil t)))
  (let ((solution-repo-dir (file-name-as-directory
			    (expand-file-name
			     label
			     tq-course-solutions-dir))))
    (if (file-exists-p solution-repo-dir)
	(with-current-directory
	 solution-repo-dir
	 ;; make sure we push the most recent work
	 (progn
	   (unless (string= "" (shell-command-to-string "git status --porcelain"))
	     (mygit "git add *")
	     (mygit (format "git commit -am \"committing solution to %s.\"" label))
	     (mygit "git push"))

	   (shell-command
	    (format "ssh %s perms solutions/%s + READERS @students"
		    (techela-course-techela-server tq-current-course)
		    label))))
      (error "%s not found" solution-repo-dir))))


(defun tq-close-solution (label)
  "Close student access to the solution LABEL."
  (interactive (list
		(completing-read
		 "Label: "
		 (tq-get-possible-assignments) nil t)))
  (shell-command
   (format "ssh %s perms solutions/%s - READERS @students"
	   (techela-course-techela-server tq-current-course)
	   label)))


;; * Grading functions
(defun tq-grade (label)
  "Collect and pull repos for assignment LABEL. Open the grading org-file.
This is not fast since it pulls each repo."
  (interactive (list
		(completing-read
		 "Label: "
		 (tq-get-assigned-assignments)
		 nil t)))

  ;; Now, make org-file
  (let ((grading-file (expand-file-name
		       (format "gradebook/grading-%s.org" label)
		       tq-gitolite-admin-dir)))
    (if (file-exists-p grading-file)
	(find-file grading-file)
      ;; else, we have to make one
      ;; set permissions to R for students
      (tq-collect label)
      (message "%s has been collected" label)

      (tq-pull-repos label)
      (message "%s has been pulled" label)

      (unless (file-exists-p (expand-file-name
			      "gradebook"
			      tq-gitolite-admin-dir))
	(make-directory (expand-file-name
			 "gradebook"
			 tq-gitolite-admin-dir)
			t))
      (find-file grading-file)
      (insert "#+TITLE: Grading
#+AUTHOR: " (user-full-name) "
#+DATE: " (format-time-string "[%Y-%m-%d %a]" (current-time)) "

* Grading for " label " [/]\n")
      ;; randomize the userids so they get graded in a different order
      ;; each time. This is to reduce systematic variation in which
      ;; students get graded first and last.
      (dolist (userid (shuffle (tq-get-userids)))
	(let* ((repo-name (tq-get-repo-name label userid))
	       (repo-dir (file-name-as-directory
			  (expand-file-name
			   repo-name
			   tq-root-directory)))
	       (student-org-file (expand-file-name
				  (concat label ".org")
				  repo-dir)))
	  (if (file-exists-p student-org-file)
	      (insert (format "** TODO [[%s][%s]]\n"
			      (file-relative-name
			       student-org-file
			       (expand-file-name "gradebook"
						 tq-gitolite-admin-dir))
			      repo-name))
	    ;; missing org file
	    (insert (format "** TODO %s missing\n" repo-name)))))

      ;; loop is over. Put in last section
      (insert (format "
* Finishing up
1. Create the [[elisp:(tq-summarize \"%s\")][summary report]]
" label)
	      (format "
2. [[elisp:(tq-return \"%s\")][Return the assignments]]
" label)
	      (format "
3. Check status


")
	      (format "
4. [[elisp:tq-commit-gradesheet][Save and push this file]]" grading-file grading-file))))
  (grade-mode))


(defun tq-commit-gradesheet ()
  "Commit gradesheet and the histogram that should be with it."
  (interactive)
  (let* ((fname (file-name-nondirectory (buffer-file-name)))
	 (assignment (replace-regexp-in-string "grading-\\|\\.org" "" fname))
	 (hist (concat assignment "-hist.png")))
    (mygit (format "git add %s" hist))
    (mygit (format "git commit %s -m \"adding %s histogram\""
		   hist assignment))
    (save-buffer)
    (mygit (format "git add %s" (buffer-file-name)))
    (mygit (format "git commit %s -m \"save changes in %s gradesheet.\""
		   (buffer-file-name)
		   assignment))
    (mygit "git push")))


(defun tq-summarize (label)
  "Insert a summary of grades for assignment LABEL."
  (forward-line 2)
  (insert (format "#+tblname: summary-%s\n" label)
	  "| userid | grade |\n|-\n")
  (dolist (userid (sort (tq-get-userids) 'string-lessp))
    (let* ((label-dir (file-name-as-directory
		       (expand-file-name
			label
			tq-course-student-work-dir)))
	   (repo (f-filename (tq-get-repo-name label userid)))
	   (repo-dir (file-name-as-directory
		      (expand-file-name
		       repo
		       label-dir)))
	   (org-file (expand-file-name
		      (concat label ".org") repo-dir))
	   (grade))
      (message "label-dir %s
repo %s
repo-dir %s
org-file %s" label-dir repo repo-dir org-file)
      (message "looking at %s" org-file)
      (setq grade (gb-get-grade org-file))
      (insert (format "| %15s | %s |\n" (format "[[%s][%s]]" (file-relative-name org-file) userid) grade))))
  (insert "\n\n")
  ;; realign table
  (previous-line 3)
  (org-ctrl-c-ctrl-c)
  (forward-line 3)
  (insert (format "
#+BEGIN_SRC python :var data=summary-%s
import matplotlib.pyplot as plt
grades = [x[1] for x in data if x[1] is not 'nil']

plt.hist(grades, 20)
plt.savefig('%s-hist.png')
# [[./%s-hist.png]]

import numpy as np
print('Average grade = {}'.format(np.mean(grades)))
print('Std Dev = {}'.format(np.std(grades)))
#+END_SRC\n" label label label))
  (previous-line 2)
  (org-ctrl-c-ctrl-c))


;; * Syllabus data
(defun tq-get-categories ()
  "Return categories from the syllabus."
  (let ((table (with-current-buffer
		   (find-file-noselect tq-course-syllabus)
		 (beginning-of-buffer)
		 (re-search-forward "#\\+tblname:\\s-*categories")
		 (forward-line)
		 (org-table-to-lisp))))
    (mapcar (lambda (x) (car x)) (cddr table))))


(defun tq-get-categories-weights ()
  "Read categories and weights from the syllabus. Returns an alist."
  (let ((table (with-current-buffer
		   (find-file-noselect tq-course-syllabus)
		 (beginning-of-buffer)
		 (re-search-forward "#\\+tblname:\\s-*categories")
		 (forward-line)
		 (org-table-to-lisp))))
    ;; cddr removes the headings and hline
    (mapcar (lambda (x)
	      (cons (car x) (string-to-number (nth 1 x))))
	    (cddr table))))


(defun tq-get-assigned-assignments ()
  "Return a list of assignment labels from the syllabus.
Assignments are headings that are tagged with :assignment:. The assignment is
a link in the heading."
  (interactive)
  (with-current-buffer (find-file-noselect tq-course-syllabus)
    (org-map-entries
     (lambda ()
       (org-entry-get (point) "CUSTOM_ID"))
     "assignment")))


;; * course status command
(defun tq-status ()
  "Show the course status dashboard."
  (interactive)
  (switch-to-buffer (get-buffer-create "*ta-status*"))
  (erase-buffer)
  (insert-file-contents (expand-file-name "templates/admin-status.org"
					  (file-name-directory
					   (locate-library "techela"))))
  (org-mode)
  (org-babel-execute-buffer)
  (org-cycle '(16)))


;; (defun tq-status ()
;;   "Switch to *techela-admin* and show git status on the course."
;;   (interactive)
;;   (switch-to-buffer (get-buffer-create "*techela-admin*"))
;;   (read-only-mode -1)
;;   (erase-buffer) 
;;   (insert "#+STARTUP: showall\n")
;;   (insert (format "#+TITLE: %s\n" (techela-course-title tq-current-course)))
;;   (with-current-directory
;;    tq-gitolite-admin-dir
;;    (let* ((git-status (shell-command-to-string "git status --porcelain"))
;; 	  (clean (string= "" git-status))
;; 	  (commits (tq-git-n-commits))
;; 	  (nlocal (nth 0 commits))
;; 	  (nremote (nth 1 commits)))

;;      (if clean
;; 	 (progn
;; 	   (insert (format "* gitolite-admin is clean %s\n"
;; 			   (format "(↑%s|↓%s)" nlocal nremote)))
;; 	   (when (> nlocal 0)
;; 	     (insert "#+BEGIN_SRC emacs-lisp
;;  (with-current-directory tq-gitolite-admin-dir
;;    (mygit \"git push\")
;;    (tq-status))
;; #+END_SRC

;; "))

;; 	   (when (> nremote 0)
;; 	     (insert "#+BEGIN_SRC emacs-lisp
;;  (with-current-directory tq-gitolite-admin-dir
;;    (mygit \"git pull\")
;;    (tq-status))
;; #+END_SRC

;; ")))

;;        ;; Dirty folder
;;        (insert (format (concat "* gitolite-admin is "
;; 			       (propertize "dirty" 'font-lock-face '(:foreground "red"))
;; 			       " %s
;;   :PROPERTIES:
;;   :VISIBILITY: folded
;;   :END:
;; git status:
;; %s") (format "(↑%s|↓%s)" nlocal nremote) git-status))

;;        (when (> nremote 0)
;; 	 (insert "#+BEGIN_SRC emacs-lisp
;;  (with-current-directory tq-gitolite-admin-dir
;;    (mygit \"git pull\")
;;    (tq-status))
;; #+END_SRC

;; "))
;;        (insert "

;; #+BEGIN_SRC emacs-lisp
;;  (with-current-directory tq-gitolite-admin-dir
;;    (mygit \"git add *\")
;;    (mygit \"git commit -m \\\"committing everything\\\"\")
;;    (mygit \"git push\")
;;    (tq-status))
;; #+END_SRC

;; "))))

;;   ;; Now check the course directory status
;;   (with-current-directory
;;    tq-course-directory
;;    (let* ((git-status (shell-command-to-string "git status --porcelain"))
;; 	  (clean (string= "" git-status))
;; 	  (commits (tq-git-n-commits))
;; 	  (nlocal (nth 0 commits))
;; 	  (nremote (nth 1 commits)))

;;      (if clean
;; 	 (progn
;; 	   (insert (format "* Course is clean %s\n"
;; 			   (format "(↑%s|↓%s)" nlocal nremote)))
;; 	   (when (> nlocal 0)
;; 	     (insert "#+BEGIN_SRC emacs-lisp
;;  (with-current-directory tq-course-directory
;;    (mygit \"git push\")
;;    (tq-status))
;; #+END_SRC

;; "))

;; 	   (when (> nremote 0)
;; 	     (insert "#+BEGIN_SRC emacs-lisp
;;  (with-current-directory tq-course-directory
;;    (mygit \"git pull\")
;;    (tq-status))
;; #+END_SRC

;; ")))
;;        ;; Dirty course
;;        (insert (format (concat "* Course is "
;; 			       (propertize "dirty" 'font-lock-face '(:foreground "red"))
;; 			       " %s

;;   :PROPERTIES:
;;   :VISIBILITY: folded
;;   :END:
;; git status:
;; %s") (format "(↑%s|↓%s)" nlocal nremote) git-status))

;;        (insert "

;; #+BEGIN_SRC emacs-lisp
;; ;; do this with caution!!!
;;  (with-current-directory tq-course-directory
;;    (mygit \"git add *\")
;;    (mygit \"git commit -m \\\"committing everything\\\"\")
;;    (mygit \"git push\")
;;    (tq-status))
;; #+END_SRC

;; "))))

;; ;;; now we get each assignment and solution
;;   (insert "* Assignment statuses
;;   :PROPERTIES:
;;   :VISIBILITY: folded
;;   :END:

;; ")
;;   (dolist (assignment (tq-get-possible-assignments)) 
;;     ;; check assignment status
;;     (let ((label assignment)
;; 	  (git-assignment-status)
;;   	  (git-solution-status)
;;   	  (header "")
;;   	  (body ""))

;;       (setq header (format "** %s %s" label
;;   			   (if (-contains? (tq-get-assigned-assignments) label)
;;   			       (propertize " (assigned)" 'font-lock-face
;; 					   '(:foreground "forestgreen"))
;;   			     " (not assigned)")))

;;       ;; get assignment status
;;       (with-current-directory
;;        (expand-file-name label tq-course-assignments-dir)
;;        (setq git-assignment-status (shell-command-to-string "git status --porcelain"))

;;        ;; link to the assignment.
;;        (setq body (concat
;;   		   body
;;   		   (format "\n  assignment [[file:%s][%s]]\n"
;;   			   (expand-file-name
;;   			    (concat label ".org") (expand-file-name
;;   						   label tq-course-assignments-dir))
;;   			   (concat label ".org"))))

;;        (if (string= "" git-assignment-status)
;;   	   (setq header (concat header " clean |"))
;;   	 (setq header (concat header " " (propertize "dirty" 'font-lock-face
;; 						     '(:foreground "red")) " |"))
;;   	 (setq body (concat
;;   		     body
;;   		     (shell-command-to-string "git status")
;;   		     (format "
;; #+BEGIN_SRC emacs-lisp
;;    (with-current-directory (expand-file-name \"%s\" tq-course-assignments-dir)
;;      (mygit \"git add *\")
;;      (mygit \"git commit -m \\\"committing everything\\\"\")
;;      (mygit \"git push\")
;;      (tq-status))
;; #+END_SRC
;;   " label)
;;   		     "\n"))))

;;       ;; solution
;;       (if (file-exists-p (expand-file-name label tq-course-solutions-dir))
;;   	  (with-current-directory
;;   	   (expand-file-name label tq-course-solutions-dir)
;;   	   (setq git-solution-status (shell-command-to-string "git status --porcelain"))
;;   	   (setq body (concat
;;   		       body
;;   		       (format "\n  solution [[file:%s][%s]]\n"
;;   			       (expand-file-name
;;   				(concat label ".org") (expand-file-name
;;   						       label tq-course-solutions-dir))
;;   			       (concat label ".org"))))

;;   	   (if (string= "" git-solution-status)
;;   	       (setq header (concat header " solution clean |"))
;;   	     (setq header (concat header " solution " (propertize "dirty"
;; 								  'font-lock-face
;; 								  '(:foreground "red")) " |"))

;;   	     (setq body (concat
;;   			 body
;;   			 (shell-command-to-string "git status")
;;   			 (format "
;;   #+BEGIN_SRC emacs-lisp
;;    (with-current-directory (expand-file-name \"%s\" tq-course-solutions-dir)
;;      (mygit \"git add *\")
;;      (mygit \"git commit -m \\\"committing everything\\\"\")
;;      (mygit \"git push\")
;;      (tq-status))
;;   #+END_SRC
;;   " label)))))
;;   	;; no solution found
;;   	(setq header (concat header " no solution"))
;;   	(setq body (concat
;;   		    body
;;   		    (format "  [[elisp:(tq-create-solution \"%s\")][Create/edit solution]]\n" label))))

;;       ;; for each assignment
;;       (insert header "\n" body "\n")))
;;   ;; now menu options
;;   (insert "
;; * Menu of options

;;   [[elisp:tq-status][Refresh]]

;; ** Admin Actions

;; - [[elisp:(find-file tq-gitolite-admin-dir)][Open the admin directory]]

;; - [[elisp:(find-file (expand-file-name \"gradebook\" tq-gitolite-admin-dir))][Open the gradebook directory]]

;; ** Course Actions

;; - [[elisp:(find-file tq-course-directory)][Open the course directory]] [[elisp:(find-file (expand-file-name \"syllabus.org\" tq-course-directory))][Syllabus]]

;; - [[elisp:(tq-roster)][Send email]]

;; - [[elisp:(find-file (expand-file-name \"roster.org\" tq-gitolite-admin-dir))][Open the roster.org]] [[elisp:tq-update-git-roster][Update gitolite roster]] (run after you change roster.org)

;; - [[elisp:tq-check-pub-keys][Check ssh keys]]

;; ** Assignment Actions

;; - [[elisp:(find-file tq-course-assignments-dir)][Open the assignments directory]]
;; - [[elisp:(find-file tq-course-student-work-dir)][Open student work directory]]
;; - [[elisp:tq-pull-repos][Update student repos]] (pulls them all locally.)

;; - [[elisp:tq-create-assignment][Create or edit an assignment]]
;; - [[elisp:tq-create-solution][Create or edit solution]]
;; - [[elisp:tq-release-solution][Release a solution (give students read-access)]]  [[elisp:tq-close-solution][Close a solution (remove read access)]]

;; - [[elisp:tq-create-assignment-repos][Create class repos for an assignment]] (no student access until you assign it.)

;; - [[elisp:tq-assign-assignment to class][Assign an assignment]] (give students RW access)
;; - [[elisp:tq-collect][Collect an assignment from class]] (change students to R access. Does not pull.)
;; - [[elisp:tq-pull-repos][Pull an assignment from class]] (get local copies of assignment. Does not change permissions.)


;; - [[elisp:tq-grade][Grade an assignment for class]] (collect and pull repos. create grading list)
;; - [[elisp:tq-return][Return an assignment to class]] (push local copies to server)

;; - [[elisp:tq-helm-gradebook][Gradebook]]

;; *** Individual Student Actions

;; - [[elisp:tq-assign-to][Assign assignment to a student. give RW access]]
;; - [[elisp:tq-collect-from][Collect assignment from a student. Make R access]]
;; - [[elisp:tq-open-assignment][Open a student assignment. Pulls first.]]
;; - [[elisp:tq-return-to][Return your changes in an assignment to a student]]

;; - [[elisp:tq-email][Email a student]]")
;;   (goto-char (point-min))
;;   (org-mode)
;;   (read-only-mode +1))

;; * Repo status commands

(defun tq-repos-status (label)
  "List status of repos for assignment LABEL."
  (interactive (list
		(completing-read
		 "Label: "
		 (tq-get-assigned-assignments)
		 nil t)))

  (switch-to-buffer (get-buffer-create (format "* %s repos *" label)))
  (erase-buffer)

  (dolist (userid (tq-get-userids))
    (let* ((dir (expand-file-name
		 (format "%s-%s" userid label)
		 (expand-file-name
		  label
		  (expand-file-name
		   "student-work"
		   (expand-file-name
		    tq-current-course
		    (expand-file-name "~/techela-admin"))))))
	   (result)
	   (n-commits) (n-modified) (n-untracked)
	   (link (format "[[elisp:(with-current-directory \"%s\" (ansi-term \"/bin/bash\"))][%s]]"
			 dir userid))
	   (status) ; clean/dirty
	   (s-commits) ; local/remote commits
	   (s-modified)
	   (s-untracked))
      (if (file-exists-p dir)

	  (with-current-directory
	   dir
	   ;; c
	   (setq result (shell-command-to-string "git status --porcelain")
		 n-commits (tq-git-n-commits)  ; (local remote)
		 n-modified (tq-git-n-modified-files)
		 n-untracked (tq-git-n-untracked-files))

	   (if (string= "" result)
	       (setq status (propertize " clean"
					'font-lock-face
					'(:foreground "forestgreen")))
	     (setq status (propertize " dirty"
				      'font-lock-face
				      '(:foreground "red"))))

	   (setq s-commits
		 (concat
		  (format "%s%s"
			  (if (not (= 0 (nth 0 n-commits)))
			      (propertize
			       "  Local "
			       'font-lock-face
			       '(:foreground "red"))
			    "  Local ")
			  (nth 0 n-commits))
		  (format "%s%s"
			  (if (not (= 0 (nth 0 n-commits)))
			      (propertize
			       "  Remote "
			       'font-lock-face '(:foreground "red"))
			    "  Remote ")
			  (nth 1 n-commits))))

	   (setq s-modified (format "  Modified=%s" n-modified)
		 s-untracked (format "  Untracked=%s" n-untracked))

	   (insert (format
		    "- %-20s %s %s %s %s\n"
		    link
		    status
		    s-commits
		    s-modified
		    s-untracked)))
	;; missing directory
	(insert (format "- %20s Missing\n" link)))))
  (org-mode))


;; (defun tq-pull-assignment-dirs ()
;;   "Pull all remotely known assignments to local machine.
;; This is useful on a new machine."
;;   (interactive)
;;   (dolist (assignment  (loop for line in
;; 			     (split-string
;; 			      (shell-command-to-string
;; 			       (format "%s %s info"
;; 				       (expand-file-name
;; 					"techela_ssh"
;; 					tq-root-directory)
;; 				       (techela-course-techela-server tq-current-course)))
;; 			      "\n")
;; 			     if (string-match "\\(assignments/[^[]]*.*\\)" line)
;; 			     collect (match-string 1 line)))
;;     (unless (file-exists-p
;; 	     (expand-file-name assignment tq-course-assignments-dir))
;;       (with-current-directory
;;        tq-course-assignments-dir
;;        (mygit (format "git clone %s:%s"
;; 		      (techela-course-techela-server tq-current-course)
;; 		      assignment))))))


;; (defun tq-pull-solutions-dirs ()
;;   "Pull solutions to local machine."
;;   (interactive)
;;   (dolist (assignment  (loop for line in
;; 			     (split-string
;; 			      (shell-command-to-string
;; 			       (format "%s %s info"
;; 				       (expand-file-name
;; 					"techela_ssh"
;; 					tq-root-directory)
;; 				       (techela-course-techela-server tq-current-course)))
;; 			      "\n")
;; 			     if (string-match "\\(solutions/[^[]]*.*\\)" line)
;; 			     collect (match-string 1 line)))
;;     (unless (file-exists-p
;; 	     (expand-file-name
;; 	      assignment
;; 	      tq-course-solutions-dir))
;;       (with-current-directory
;;        tq-course-solutions-dir
;;        (mygit (format "git clone %s:%s"
;; 		      (techela-course-techela-server tq-current-course)
;; 		      assignment))))))


(defun tq-add-pub-key (pubfile)
  "Add PUBFILE to gitolite-admin/keydir.
You will have an opportunity to change the name, e.g. to add an
@location to the pubkey, e.g. jkitchin@gmail.com.pub to
jkitchin@gmail.com@home.pub. This is useful if you have
downloaded the pub-key from an email and want an easy way to add
it."
  (interactive (list
		(read-file-name "Pub file:" "~/Downloads/")))
  (let* ((name (concat (read-input "Key name:" (file-name-base pubfile)) ".pub"))
	 (fullname (expand-file-name name (concat tq-gitolite-admin-dir "keydir"))))

    ;; move key to gitolite-admin/keydir/name, ask for confirmation if key
    ;; already exists.
    (rename-file pubfile fullname t)
    (with-current-directory (concat tq-gitolite-admin-dir "keydir")
			    (mygit (format "git add %s" fullname))
			    (mygit (format "git commit %s -m \"new key for %s\""
					   fullname name))
			    (mygit "git push")
			    (message "%s pushed." name))))

;; * Utility functions

(defun tq-server-repos ()
  "Retrieve a list of repos on the server.
Each element is (permission-string repo-name)."
  (mapcar (lambda (line)
	    (split-string line "\t" t))
	  (cddr (split-string (nth 1 (myssh "info"))
			      "\n" t))))


(defun tq-server-info ()
  "Show buffer with information about the techela server."
  (interactive)
  (switch-to-buffer (get-buffer-create "tq-info"))
  (erase-buffer)
  (insert (s-join "\n" (mapcar (lambda (s) (s-join " " s)) (tq-server-repos)))))


(defun tq-clone-server-assignments ()
  "Locally clone or update all the assignments from the server.
This will get all assignments that are on the server, including
ones not yet assigned."
  (interactive)
  (let* ((all-repos (tq-server-repos))
	 (assignment-repos (-filter (lambda (repo-cell)
				      (string-match "assignments/[a-z].*" (nth 1 repo-cell)))
				    all-repos))
	 (default-directory tq-root-directory))
    ;; (permissions-string repo-name)
    ;; repo-names are assignments/label
    (loop for (permission-string repo-name) in assignment-repos
	  do
	  (if (not (file-directory-p repo-name))
	      ;; get it
	      (with-current-directory tq-course-assignments-dir
				      (mygit "git clone %s" repo-name))
	    (with-current-directory (expand-file-name repo-name
						      tq-root-directory)
				    (mygit "git pull origin master"))))))

(defun tq-clone-server-solutions () 
  "Locally clone or update all the assignments from the server."
  (interactive)
  (let* ((all-repos (tq-server-repos))
	 (solution-repos (-filter (lambda (repo-cell)
				    (string-match "solutions/[a-z].*" (nth 1 repo-cell)))
				  all-repos))
	 (default-directory tq-root-directory))
    ;; (permissions-string repo-name)
    ;; repo-names are assignments/label
    (loop for (permission-string repo-name) in solution-repos
	  do
	  (if (not (file-directory-p repo-name))
	      ;; get it
	      (with-current-directory tq-course-solutions-dir
				      (mygit "git clone %s" repo-name))
	    (with-current-directory (expand-file-name repo-name
						      tq-root-directory)
				    (mygit "git pull origin master"))))))


(provide 'techela-admin)

;;; techela-admin.el ends here
