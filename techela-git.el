;;; techela-git.el --- Scripting git in emacs for techela


;;; Commentary:
;;

;;; Code:
(defvar *GIT_SSH* nil
  "the wrapper script for ssh")


(defmacro with-current-directory (directory &rest body)
  "Set the working directory temporarily set to DIRECTORY and run BODY.
DIRECTORY is expanded"
  `(let ((default-directory (file-name-as-directory
			     (expand-file-name ,directory))))
     ,@body))


(defun myssh (command)
  "Use the techela_ssh script to run an ssh command.
Returns (list status output)."
  (let* ((techela-ssh (expand-file-name
		       "techela_ssh"
		       tq-root-directory))
	 (cmd (mapconcat #'identity
			 (list techela-ssh
			       (techela-course-techela-server tq-current-course)
			       command)
			 " "))
	 status output)
    (when (get-buffer "*myssh*") (kill-buffer "*myssh*"))
    (setq status (call-process-shell-command cmd nil "*myssh*"))
    (setq output (with-current-buffer "*myssh*" (buffer-string)))
    (list status output)))

(defun mygit (git-command)
  "Run GIT-COMMAND in custom environment.

For example:
 (mygit \"git clone org-course@techela.cheme.cmu.edu:course\")

Sets GIT_SSH to `*GIT_SSH*', and temporarily modifies the process
environment before running git. `*GIT_SSH*' points to a shell
script that runs ssh in batch mode.

returns (status output)"
  (interactive "sgit command: ")
  (setq *GIT_SSH*
	(format
	 "GIT_SSH=%s"
	 (expand-file-name
	  "techela_ssh"
	  tq-root-directory)))
  (let ((process-environment (cons *GIT_SSH* process-environment))
        (status) (output))
    (when (get-buffer "*mygit-process*") (kill-buffer "*mygit-process*"))
    (tq-log "\nmygit Running \"%s\"\n  CWD = %s" git-command default-directory)
    (setq status (call-process-shell-command git-command nil "*mygit-process*"))
    (setq output (with-current-buffer "*mygit-process*" (buffer-string)))
    (tq-log "  status = %s" status)
    (tq-log "  output = %s" output)
    (list status output)))

(defun tq-in-git-p (&optional debug)
  "Return status for whether `default-directory' is in a git repo.
Optional argument DEBUG switch to output buffer if the command fails."
  (interactive)
  (mygit "git rev-parse --is-inside-work-tree"))


(defun tq-get-num-incoming-changes ()
  "Return number of commits the remote is different than local."
  (interactive)
  (unless (tq-in-git-p)
    (error "You are not in a git repo.  We think you are in %s" default-directory))
  (mygit "git fetch origin")
  (string-to-number (nth 1 (mygit "git rev-list HEAD...origin/master --count"))))


(defun tq-clone-repo (repo)
  "Clone REPO from the techela-server into current directory if needed.
If REPO exists, do not do anything.  REPO should not have the extension .git on
it.  If you want to clone it somewhere else, use `let' to temporarily define
`default-directory'."
  (if (file-exists-p (f-filename repo))
      repo
    (when (not (= 0 (car (mygit (format "git clone %s:%s.git"
					(techela-course-techela-server tq-current-course)
					repo)))))
      (switch-to-buffer "*techela log*")
      (error "Problem cloning %s" repo))
    repo))


(defun tq-clone-and-open (repo)
  "Clone REPO and open it."
  (let ((default-directory tq-root-directory))
    (tq-clone-repo repo)
    (find-file (expand-file-name (concat repo ".org") repo))))


(defun tq-git-n-commits ()
  "Return how many commits differ locally and remotely.

\(0 0) means you are up to date as far as commits go
\(n 0) means you are ahead of the remote and need to push
\(0 n) means you are behind the remote and need to pull
\(m n) means there is a divergence, and a pull and push will be required."
  (interactive)
  ;; see what is on remote
  (mygit "git fetch")
  (let ((result
	 (mygit "git rev-list --count --left-right HEAD...origin/master"))
	(status) (output)
	(local-changes) (remote-changes))
    (setq status (nth 0 result))
    (setq output (nth 1 result))
    (setq local-changes (string-to-number (nth 0 (split-string output))))
    (setq remote-changes (string-to-number (nth 1 (split-string output))))

    (list local-changes remote-changes)))


(defun tq-git-n-untracked-files ()
  "Return number of untracked files.
These are files with ?? in them from git status --porcelain"
  (interactive)
  (let ((n 0))
    (dolist (line (split-string
		   (shell-command-to-string "git status --porcelain")
		   "\n"))
      (when (string-match "^\\?\\?" line)
	(setq n (+ 1 n))))
    n))


(defun tq-git-n-modified-files ()
  "Return number of modified, but uncommitted files.
These are files with M in them from git status --porcelain"
  (let ((n 0))
    (dolist (line (split-string
		   (shell-command-to-string "git status --porcelain")
		   "\n"))
      (when (string-match "^ M" line)
	(setq n (+ 1 n))))
    n))


(defun tq-git-buffer-tracked-p ()
  "Return if the file the buffer is visiting is tracked by git.

git ls-files filename returns an empty string if filename is not
under git control"
  (interactive)
  (not (string=
	""
	(shell-command-to-string
	 (format "git ls-files %s"
		 (file-name-nondirectory
		  (buffer-file-name)))))))


(defun tq-git-buffer-modified-p ()
  "Return if the file the buffer is visiting has been modified.

Save the buffer first.
git status --porcelain filename
returns \" M filename
\" when the file is modified."
  (interactive)
  (when (tq-git-buffer-tracked-p)
    (save-buffer)
    (string-match
     "^ M"
     (shell-command-to-string
      (format "git status --porcelain %s"
	      (file-name-nondirectory
	       (buffer-file-name)))))))


(defvar tq-git-unmerged-states
  '(("D" "D")
    ("A" "U")
    ("U" "D")
    ("U" "A")
    ("D" "U")
    ("A" "A")
    ("U" "U"))
  "List of XY states a file can be in that indicate an unmerged state.")

(defun tq-git-make-repo-clean ()
  "Handle every line in git status --porcelain and leave repo in clean state.

This may happen before or after a merge. Before a merge, we
handle each line. After a merge, we add everything at once, and
then commit the merge."
  (interactive)

  (let* ((merge-p nil)
	 (lines (loop for line in
		      (split-string
		       (shell-command-to-string "git status --porcelain") "\n")
		      when (not (string= "" line))
		      collect
		      (let* ((X (substring line 0 1))
			     (Y (substring line 1 2))
			     (PATHS (split-string (substring line 3)))
			     (FROM (nth 0 PATHS))
			     ;; for a rename there will be PATH1 -> PATH2
			     (TO (if (= 3 (length PATHS)) (nth 2 PATHS) nil)))
			(when (-contains? tq-git-unmerged-states (list X Y))
			  (setq merge-p t))
			(message "%s" (list X Y FROM TO line))
			(list X Y FROM TO line)))))

    (if merge-p
	;; we need to add everything, and commit it.
	(progn
	  (message "Processing an unmerged state")
	  (mygit (format "git add *"))
	  (mygit (format "git commit -am \"committing a merge\"")))

      ;; we are not in a merge, so we just handle each line.  now we
      ;; loop through the lines and handle them. We try to handle
      ;; individual lines so we can create more meaningful, single
      ;; file commits.
      (loop for (X Y FROM TO LINE) in lines
	    do
	    (message "handling %s" LINE)
	    (cond
	     ;; untracked files get added and committed.
	     ((equal (list X Y) '("?" "?"))
	      (mygit (format "git add %s" FROM))
	      (mygit
	       (format "git commit %s -m \"adding untracked file: %s.\""
		       FROM FROM)))

	     ;; user rename
	     ((equal (list X Y) '("R" " "))
	      (mygit (format "git commit %s -m \"rename %s to %s\"" FROM FROM TO))
	      (mygit (format "git add %s" TO))
	      (mygit (format "git commit %s -m \"adding %s\"" TO TO)))

	     ;; rename and modify
	     ((equal (list X Y) '("R" "M"))
	      ;; commit the rename
	      (mygit (format "git commit %s -m \"rename %s to %s\"" FROM FROM TO))
	      (mygit (format "git commit %s -m \"changes in %s\"" TO TO)))

	     ;; added file
	     ((equal (list X Y) '("A" " "))
	      (mygit (format "git commit %s -m  \"Adding %s\"" FROM FROM)))

	     ;; deleted file
	     ((equal (list X Y) '(" " "D"))
	      (mygit (format "git commit %s -m \"Deleting %s\"" FROM FROM)))

	     ;; modified file
	     ((or (string= X "M")
		  (string= Y "M"))
	      (mygit (format "git add %s" FROM))
	      (mygit (format "git commit %s -m \"changes in %s\"" FROM FROM)))

	     ;; catch everything else
	     (t
	      (mygit (format "git add %s" FROM))
	      (mygit (format "git commit %s -m \"%s\"" FROM LINE))))))))


(defun tq-git-update-file ()
  "Update the current file.

This is tricky because we cannot just pull a file. We have to save all files, commit them, then pull. It may be the case that this causes a merge conflict, which we then need to address.

The strategy is to check git status --porcelain first, and get the repo into a clean state. Then we do the pull. Then, we check again and get back to a clean state if needed."
  (interactive)

  (save-some-buffers t t) ; make sure all files are saved.
  (with-current-directory
   ;; this switches us to the top git level which is where all the
   ;; paths are relative to.
   (s-trim
    (shell-command-to-string "git rev-parse --show-toplevel"))
   ;; first clean the repo
   (tq-git-make-repo-clean)

   ;; Next we are going to fetch
   (mygit "git fetch")

   ;; then merge, we assume with origin/master
   (mygit "git merge origin/master -m \"merging origin/master in\"")

   ;; there may be merge conflicts. we take them, and make the repo
   ;; clean again.
   (tq-git-make-repo-clean)

   ;; update this buffer
   (revert-buffer t t)))

(provide 'techela-git)

;;; techela-git.el ends here
