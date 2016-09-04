;;; techela-grade.el --- Functions for grading techela assignments

;;; Commentary:
;; 

;;; Code:

(defvar tq-rubrics '(("homework" . (("\"technical\"" . 0.7)
				    ("\"presentation\"" . 0.3)))
		     ("exam" . (("\"technical\"" . 0.7)
				("\"presentation\"" . 0.3)))
		     ("multiple-choice" . (("\"participation\"" . 1.0)))
		     ("participation" . (("\"participation\"" . 1.0))))
  "List of rubrics for assignments. Each element should be a list
  of components in alist form.")


(defvar gb-MULTIPLIERS
  '(("A++" . 1.0)
    ("A+" . 0.95)
    ("A" . 0.9)
    ("A-" . 0.85)
    ("A/B" . 0.8)
    ("B+" . 0.75)
    ("B" . 0.7)
    ("B-" . 0.65)
    ("B/C" . 0.6)
    ("C+" . 0.55)
    ("C" . 0.5)
    ("C-" . 0.45)
    ("C/D" . 0.4)
    ("D+" . 0.35)
    ("D" . 0.3)
    ("D-" . 0.25)
    ("D/R" . 0.2)
    ("R+" . 0.15)
    ("R" . 0.1)
    ("R-" . 0.05)
    ("R--" . 0.0)
    ("P" . 1.0)    ;; Pass
    ("F" . 0.0)    ;; Fail
    ("WAIVED" . nil))
  "Numeric multipliers for letter grades.")


(defun gb-fraction-to-lettergrade (fraction)
  "Return the letter grade associated with FRACTION.
For example, 0.79 is associated with a B+, and 0.81 is associated
with an A/B."
  (let ((tuples (reverse gb-MULTIPLIERS))
	(last-letter-grade))
    (setq tuples (remove '("WAIVED" . nil) tuples))
    (setq tuples (remove '("P" . 1.0) tuples))
    (setq tuples (remove '("F" . 1.0) tuples))
    
    (catch 'grade
      (dolist (tuple tuples)
	(let ((lettergrade (car tuple))
	      (multiplier (cdr tuple)))
	  (when multiplier
	    (when (< fraction multiplier)
	      (throw 'grade last-letter-grade))
	    (setq last-letter-grade lettergrade)))))))


(defun gb-ontime-p ()
  "Return if the assignment is on time.
This means it is turned in before 23:59:59 on the day it is due." 
  (let ((duedate (replace-regexp-in-string
		  ">"
		  " 23:59:59>" (gb-get-filetag "DUEDATE")))
	(turned-in (format-time-string
		    "[%Y-%m-%d %a %H:%M]"
		    (date-to-time (gb-get-filetag "TURNED-IN")))))
    (org-time< turned-in duedate)))


;;;###autoload
(defun gb-grade ()
  "Insert a grade in the buffer.
Uses the rubric for the assignment and calculates the overall
grade. This assumes the assignment label is the filename you are
in."
  (interactive)
  (save-window-excursion
    (let ((rubric)
	  (label (s-trim (substring-no-properties
			  (gb-get-filetag "ASSIGNMENT"))))
	  (cb (current-buffer))
	  (tbuf (find-file-noselect (expand-file-name "syllabus.org" tq-course-directory))))
      (unless (-contains? (tq-get-assigned-assignments) label)
	(error "%s is not an assignment" label))

      ;; get the rubric from the syllabus, to make sure it has not been
      ;; altered by a student
      (set-buffer tbuf)
      (goto-char (point-min))
      (org-open-link-from-string
       ;; there must be a section with a custom_id in the syllabus
       (format "[[#%s]]" label) tbuf)
      (setq rubric (read (org-entry-get (point) "RUBRIC")))
      (set-buffer cb)
      (kill-buffer tbuf)

      ;; Now, loop over rubric and enter grades for each category
      (setq categories (mapcar (lambda (x) (car x)) rubric))
      (setq LGS (mapcar (lambda (cell)
			  (completing-read
			   (concat
			    (cond
			     ((symbolp (car cell))
			      (symbol-name (car cell)))
			     ((stringp (car cell))
			      (car cell)))
			    ": ")
			   (mapcar (lambda (x) (car x)) gb-MULTIPLIERS)))
			rubric))

      (setq multipliers (mapcar (lambda (LG) (cdr (assoc LG gb-MULTIPLIERS)))
				LGS))
      (setq weights (mapcar (lambda (x) (cdr x)) rubric))
      (setq grade (reduce '+ (cl-mapcar (lambda (weight multiplier) (* weight multiplier))
					weights multipliers)))

      (goto-char (point-min))
      (unless (re-search-forward "* Grade" (point-max) 'end)
	(insert "\n* Grade\n"))
      
      (cl-mapcar (lambda (category grade) (gb-set-filetag category grade))
		 categories LGS)
      (gb-set-filetag "GRADE" (format "%1.3f" grade))
      (gb-set-filetag "GRADED-BY" user-full-name)
      ;; (unless (gb-ontime-p)
      ;; 	(gb-set-filetag "LATE" "Your assignment was late. You may be subject to a 50% penalty in the future."))
      (save-buffer)
      (kill-buffer))))


(defun gb-get-grade (fname)
  "Open file FNAME and get grade." 
  (when (and (file-exists-p fname) (file-readable-p fname))
    (with-temp-buffer
      (insert-file-contents fname)
      (org-mode)
      (gb-get-filetag "GRADE"))))


(defun gb-set-filetag (tag value)
  "Set filetag TAG to VALUE.
If VALUE is nil, remove the filetag."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (format "#\\+%s:" tag) (point-max) 'end)
	;; replace existing filetag
	(progn
	  (beginning-of-line)
	  (kill-line)
	  (when value
	    (insert (format "#+%s: %s" tag value))))
      ;; add new filetag
      (if (looking-at "^$") 		;empty line
	  ;; at beginning of line
	  (when value
	    (insert (format "#+%s: %s" tag value)))
	;; at end of some line, so add a new line
	(when value
	  (insert (format "\n#+%s: %s" tag value)))))))


(defun gb-get-filetag (tag)
  "Return value for TAG in the org-file."
  ;; (setq kwds (org-element-map (org-element-parse-buffer 'element) 'keyword
  ;;	       (lambda (keyword)
  ;;		 (cons (org-element-property :key keyword)
  ;;		       (org-element-property :value keyword)))))
  ;; (cdr (assoc tag kwds))

  ;; Apr 13, 2016 I am not sure why the code above stopped working. It seems to
  ;; be the parse buffer command that hangs.
  (save-excursion
    (goto-char (point-min))
    (if	(re-search-forward (format "#\\+%s:\\(.*\\)" tag) (point-max) t)
	(match-string 1)
      nil)))


;; * grade-mode
;;;###autoload
(defun gb-save-and-close-buffer ()
  "Save current buffer and kill it."
  (interactive)
  (save-buffer)
  (kill-buffer))


;;;###autoload
(defun gb-return ()
  "Return current buffer.
This should commit changes, and push back to the server.
Assumes you are on an assignment"
  (interactive)
  (tq-return-to
   (gb-get-filetag "ASSIGNMENT")
   ;; hackery to get userid from the directory name. Basically
   ;; splitting the path to get the directory this buffer is in,
   ;; splitting that directory by "-" and taking the first part as the
   ;; userid
   (car
    (split-string
     (car
      (last (butlast (split-string (buffer-file-name) "/")))) "-"))))


(defvar grade-mode-map
  (let ((gb-map (make-sparse-keymap))) 
    (define-key gb-map (kbd "M-s g") 'gb-grade)
    (define-key gb-map (kbd "M-s r") 'gb-return)
    (define-key gb-map (kbd "M-s q") 'gb-save-and-close-buffer)
    gb-map)
  "Keymap for function `grade-mode'.")


(easy-menu-define grade-menu grade-mode-map "Grade menu"
  '("grade"
    ["Assign grade" gb-grade t]
    ["Return assignment" gb-return t]
    ["Save and close buffer" gb-save-and-close-buffer t]))


(define-minor-mode grade-mode
  "Minor mode for grade

\\{grade-mode-map}"
  :lighter " grade"
  :global t
  :keymap grade-mode-map)

(provide 'techela-grade)

;;; techela-grade.el ends here
