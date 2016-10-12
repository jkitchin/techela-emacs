;;; techela-gradebook.el --- Gradebook



;;; Commentary:
;; This is a library of function for computing course grades in techela.

;;; Code:

(defun tq-get-graded-assignments ()
  "Return a list of assignments from the syllabus.
Assignments are headings that are tagged with :assignment: and
which are GRADED.  This returns a list of (assignment points
category)" 
  (save-current-buffer
    (with-current-buffer
	(find-file-noselect tq-course-syllabus)
      (org-map-entries
       (lambda ()
	 (list
	  (org-entry-get (point) "CUSTOM_ID")
	  :points (string-to-number (org-entry-get (point) "POINTS"))
	  :category (org-entry-get (point) "CATEGORY")))
       "+assignment+TODO=\"GRADED\""))))


(defun tq-get-assignments-in-category (category)
  "Get a list of assignments, points and categories for CATEGORY."
  (-filter
   (lambda (x)
     (string= (plist-get (cdr x) :category) category))
   (tq-get-graded-assignments)))


;;;###autoload
(defun tq-insert-student-work-link (label userid)
  "insert link  to open a student assignment."
  (interactive
   (list
    (completing-read "Label: " (tq-get-assigned-assignments) nil t)
    (completing-read "Userid: " (tq-get-userids))))
  (let ((fname (expand-file-name
		(format "%s.org" label)
		(expand-file-name
		 (format "%s-%s" userid label)
		 (expand-file-name
		  label
		  tq-course-student-work-dir)))))
    (insert (format "[[%s][%s]]"
		    fname
		    (format "%s-%s.org" userid label)))))

;;;###autoload
(defun tq-get-user-grade (label userid)
  "Returns the numeric fractional grade in a file."
  (interactive
   (list
    (completing-read "Label: " (tq-get-assigned-assignments) nil t)
    (completing-read "Userid: " (tq-get-userids))))

  (let ((fname (expand-file-name
		(format "%s.org" label)
		(expand-file-name
		 (format "%s-%s" userid label)
		 (expand-file-name
		  label
		  tq-course-student-work-dir)))))
    (when (file-exists-p fname)
      (string-to-number (or (gb-get-grade fname) "")))))


;;;###autoload
(defun tq-get-user-grades (userid)
  "Get list of p-lists of user grades for each assigned, graded assignment.
:label is the assignment name
:fractional-grade is the fractional grade
:possible-points points the problem is worth
:category is the category
:category-weight is the category weight
:category-weighted-points is the fractional grade * points * category weight"
  (interactive
   (list
    (completing-read "Userid: " (tq-get-userids))))

  ;; body of function
  (mapcar
   (lambda (label)
     (let* ((e (assoc label (tq-get-graded-assignments)))
	    (grade (tq-get-user-grade label userid))
	    (points (plist-get (cdr e) :points))
	    (category (plist-get (cdr e) :category))
	    (category-weight (cdr (assoc category (tq-get-categories-weights)))))
       ;; here we construct the p-list to return for each assighment
       (list
	:label label
	:fractional-grade grade
	:possible-points points
	:category category
	:category-weight category-weight
	:category-weighted-points (if grade
				      (*  grade points category-weight)
				    0))))
   ;; map over list of graded assignment labels
   (mapcar 'car (tq-get-graded-assignments))))


;;;###autoload
(defun tq-get-user-overall-grade (userid)
  "Return list of name, userid, fractional and letter grade for USERID."
  (interactive
   (list
    (completing-read "Userid: " (tq-get-userids))))
  
  (let* ((grades (tq-get-user-grades userid))
	 ;; (earned (mapcar (lambda (x)
	 ;; 		   (* (plist-get x :fractional-grade)
	 ;; 		      (plist-get x :possible-points)))
	 ;; 		 grades))
	 ;; (possible (mapcar (lambda (x)
	 ;; 		     (plist-get x :possible-points))
	 ;; 		   grades))
	 
	 (earned (mapcar (lambda (x)
			   (plist-get x :category-weighted-points))
			 grades))
	 (possible (mapcar (lambda (x)
			     (* (plist-get x :category-weight)
				(plist-get x :possible-points)))
			   grades))
	 (fgrade (/ (apply '+ earned) (apply '+ possible)))
	 (lg (gb-fraction-to-lettergrade fgrade))
	 (name (plist-get
		(cdr (assoc userid (tq-roster-data))) :name)))
    (list
     name
     userid
     fgrade	; fractional grade
     (gb-fraction-to-lettergrade fgrade))))


;;;###autoload
(defun tq-get-user-category-grades (userid category)
  "Return list of USERID, assignment, points and grade for each graded assignment in CATEGORY."
  (interactive
   (list
    (completing-read "Userid: " (tq-get-userids))
    (completing-read "Category: " (tq-get-categories))))
  (mapcar
   (lambda (x)
     (list
      userid
      (plist-get x :label)
      (plist-get x :possible-points)
      (plist-get x :fractional-grade)))
   (-filter
    (lambda (x)
      (string= category (plist-get x :category)))
    (tq-get-user-grades userid))))

;; * Collect responses
;;;###autoload
(defun tq-collect-responses (assignment label)
  "Collect responses for the ASSIGNMENT and LABEL within ASSIGNMENT."
  (interactive "sAssignment: \nsLabel: ")
  ;; pull repos. we do not change permissions with this, in case
  ;; you want to do updates
  ;; this is a slow, serial step
  ;;(tq-pull-repos assignment)

  ;; now we get the files. they are in ~/techela-admin/course-name/student-work/assignment/*/label.dat
  (let* ((student-work-dir (expand-file-name
			    "student-work"
			    (expand-file-name
			     tq-current-course
			     (expand-file-name "~/techela-admin"))))
	 (files (f-entries (expand-file-name
			    assignment student-work-dir)
			   (lambda (f)
			     (s-ends-with?
			      (concat label ".dat")
			      (file-name-nondirectory f)
			      ))
			   t)) ; recursive
	 (responses (mapcar (lambda (f)
			      (with-temp-buffer
				(insert-file-contents f)
				(s-trim (buffer-string))))
			    files))
	 (COUNTS (counts responses))
	 (result '()))
    (add-to-list 'result '("category" "count") t)
    (add-to-list 'result 'hline t)
    (dolist (c COUNTS)
      (add-to-list 'result (list (car c) (cdr c)) t))
    result))


;; * helm interface to gradebook
(defun tq-gradebook-candidates ()
  (loop for row in (tq-roster-data)
	collect
	(let* ((userid (car row))
	       (plist (cdr row))
	       (name (plist-get plist :name)))
	  (cons (format "%10s | %s" userid name) userid))))


(defun tq-student-overall-grade (userid)
  "Print overall grade for USERID in a buffer."
  (interactive (list (completing-read "Userid: "
				      (mapcar #'car (tq-roster-data)))))
  (switch-to-buffer (get-buffer-create "*gradebook*"))
  (erase-buffer)
  (insert
   (destructuring-bind (lname fname id fgrade lgrade)
       (tq-get-user-overall-grade userid)

     (format "%s %s (%s) %1.3f %s"
	     fname lname id fgrade lgrade))))


(defun tq-student-grades (userid)
  "Generates a buffer with the grades for USERID."
  (interactive (list (completing-read "Userid: "
				      (mapcar #'car (tq-roster-data)))))
  (switch-to-buffer (get-buffer-create "*gradebook*"))
  (erase-buffer)
  (insert (format "#+TITLE: Grade report for %s\n"
		  (destructuring-bind (name id fgrade lgrade)
		      (tq-get-user-overall-grade userid)
		    (format "%s (%s)
Overall grade: %1.3f %s\n"
			    name id fgrade lgrade))))
  (insert "#+tblname: grades\n")
  (insert
   (mapconcat 'identity
	      (loop for row in (tq-get-user-grades userid)
		    collect
		    (format "|%20s| %15s| %6.3f | %6s|"
			    (plist-get row :label)
			    (plist-get row :category)
			    (or (plist-get row :fractional-grade) 0)
			    (plist-get row :possible-points)))
	      "\n"))
  (goto-char (point-min))
  (org-mode))


(defun tq-student-overall-grades ()
  "Print overall grades in a buffer for each student in the roster."
  (interactive)
  (switch-to-buffer (get-buffer-create "*overall gradebook*"))
  (erase-buffer)
  (org-mode)
  (insert "| First name | Last Name | id | Grade | Letter grade |\n|-\n")
  (dolist (userid (mapcar 'car (tq-roster-data)))
    (insert
     (destructuring-bind (lname fname id fgrade lgrade)
	 (tq-get-user-overall-grade userid)

       (format "| %s | %s | [[elisp:(tq-student-grades \"%s\")][%s]] |  %1.3f | %s | \n"
	       fname lname id id fgrade lgrade))))
  (previous-line 2)
  (org-table-align)
  (goto-char (point-max)))



(defun tq-helm-gradebook ()
  "A helm interface to the gradebook."
  (interactive)
  (helm :sources '(((name . "Students")
		    (candidates . tq-gradebook-candidates)
		    (action . (("Individual grades" . tq-student-grades)
			       ("Overall grade" . tq-student-overall-grade)))))))

(provide 'techela-gradebook)

;;; techela-gradebook.el ends here
