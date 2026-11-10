;;; york-swo.el --- Export a request as a project plan and SWO workbook  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  K. C. Juntunen

;; Author: K. C. Juntunen <k.c.juntunen@yorkwwt.com>
;; Keywords: data

;;; Commentary:

;; `york-export-swo' writes the request subtree at point (as inserted
;; by `york-get-request-data') to an Excel workbook with two sheets: the
;; project plan, and the service work order that refers to it.
;;
;; A workbook is a zip archive of XML files.  The files that never
;; change (styles, logo, page setup) are kept unpacked in
;; `york-swo-template-directory'; this file writes the two sheets and
;; zips the result with the tar that comes with Windows.  The numbers
;; in the s="N" attributes below are cell styles, defined in the
;; template's xl/styles.xml.

;;; Code:

(require 'org)
(require 'calendar)
(require 'seq)
(require 'subr-x)

(defvar york-workarea)

(defvar york-swo-template-directory
	(expand-file-name "york-swo-template" user-emacs-directory)
	"Directory holding the unpacked workbook that `york-export-swo' fills in.")

(defvar york-swo-tar-program
	(expand-file-name "System32/tar.exe" (or (getenv "SystemRoot") "C:/Windows"))
	"The bsdtar that comes with Windows; unlike GNU tar, it writes zip files.")

;;;; Reading the request

(defun york-swo--property (name)
	"Return property NAME of the entry at point, or nil if it is blank."
	(let ((value (org-entry-get nil name)))
		(and value (not (string-blank-p value)) (string-trim value))))

(defun york-swo--rate ()
	"Return the rate in the Totals table of the entry at point, or ask for it."
	(let ((rate (save-excursion
								(let ((end (save-excursion (outline-next-heading) (point))))
									(when (re-search-forward "^| *Total est\\. Hours" end t)
										(forward-line 2)
										(string-to-number
										 (or (nth 1 (split-string (buffer-substring (line-beginning-position)
																																(line-end-position))
																							"|" t " *"))
												 "0")))))))
		(if (and rate (> rate 0))
				rate
			(read-number "Hourly rate: "))))

(defun york-swo--task ()
	"Return the task at point as a plist."
	(list :number (or (york-swo--property "Task") (york-swo--property "TaskNbr"))
				:description (org-get-heading t t t t)
				:target (york-swo--property "targetDate")
				:completed (york-swo--property "dateCompleted")
				:responsible (york-swo--property "Responsible")
				:notes (york-swo--property "status")
				:hours (string-to-number (or (york-swo--property "BillableHours") "0"))
				:timeline (or (york-swo--property "Timeline")
											(and (org-entry-is-done-p) "Complete"))))

(defun york-swo--request ()
	"Return the request around point as a plist.
Its :phases are (NAME . TASKS), each task a plist."
	(save-excursion
		(org-back-to-heading t)
		(while (and (not (equal (org-entry-get nil "TypeKey") "1"))
								(org-up-heading-safe)))
		(unless (equal (org-entry-get nil "TypeKey") "1")
			(user-error "Point is not inside a request"))
		(let ((title (org-get-heading t t t t))
					(client (york-swo--property "ClientAccount"))
					(rate (york-swo--rate))
					phases)
			(org-map-entries
			 (lambda ()
				 (pcase (org-entry-get nil "TypeKey")
					 ("2" (push (list (or (york-swo--property "Task") (org-get-heading t t t t)))
											phases))
					 ("3" (unless phases (push (list "") phases))
						(push (york-swo--task) (cdr (car phases))))))
			 nil 'tree)
			(list :title title :client client :rate rate
						:phases (nreverse (mapcar (lambda (phase)
																				(cons (car phase) (reverse (cdr phase))))
																			phases))))))

;;;; Writing cells

(defun york-swo--escape (text)
	"Return TEXT with the characters XML reserves replaced."
	(replace-regexp-in-string
	 "[&<>]" (lambda (c) (pcase c ("&" "&amp;") ("<" "&lt;") (_ "&gt;")))
	 text t t))

(defun york-swo--c (column row style body &optional type)
	"Return the XML of the cell at COLUMN and ROW, holding BODY.
STYLE, a number, and TYPE, a string, are optional."
	(format "<c r=\"%s%d\"%s%s>%s</c>" column row
					(if style (format " s=\"%d\"" style) "")
					(if type (format " t=\"%s\"" type) "")
					body))

(defun york-swo--cell (column row &optional style value)
	"Return the XML of the cell at COLUMN and ROW with STYLE.
VALUE is a number, a string or nil for an empty cell."
	(cond ((null value) (york-swo--c column row style ""))
				((numberp value) (york-swo--c column row style (format "<v>%s</v>" value)))
				(t (york-swo--c column row style
												(format "<is><t xml:space=\"preserve\">%s</t></is>"
																(york-swo--escape value))
												"inlineStr"))))

(defun york-swo--formula (column row style formula result)
	"Return the XML of the cell at COLUMN and ROW that computes FORMULA.
RESULT, a number or string, is what the formula comes to; it is shown
by programs that don't calculate."
	(york-swo--c column row style
							 (format "<f>%s</f><v>%s</v>" (york-swo--escape formula)
											 (if (stringp result) (york-swo--escape result) result))
							 (and (stringp result) "str")))

(defun york-swo--row (number attributes &rest cells)
	"Return the XML of row NUMBER holding CELLS; ATTRIBUTES is a string or nil."
	(format "<row r=\"%d\"%s>%s</row>" number
					(if attributes (concat " " attributes) "")
					(string-join cells)))

(defun york-swo--serial (month-day-year)
	"Return the Excel serial number of the calendar date MONTH-DAY-YEAR."
	(- (calendar-absolute-from-gregorian month-day-year)
		 (calendar-absolute-from-gregorian '(12 30 1899))))

(defun york-swo--date (value)
	"Return VALUE as an Excel date if it holds one; otherwise return VALUE."
	(if (and value
					 (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" value))
			(york-swo--serial (mapcar (lambda (n) (string-to-number (match-string n value)))
																'(2 3 1)))
		value))

;;;; The project plan sheet

(defun york-swo--plan (request)
	"Return the project plan sheet of REQUEST.
The value is (ROWS LAST-ROW RATE-ROW TASKS): the XML of the rows, the
number of the last row, the number of the row holding the rate, and
the tasks, each with the :row it landed on."
	(let* ((phases (plist-get request :phases))
				 (rate (plist-get request :rate))
				 ;; Each phase takes a blank row, a title, a header and its tasks.
				 (last-task (apply #'+ 2 (mapcar (lambda (phase) (+ 2 (length phase))) phases)))
				 (total-row (+ last-task 3))
				 (rate-row (1+ total-row))
				 (row 2)
				 (rows (list (york-swo--row 2 "ht=\"18.75\""
																		(york-swo--cell "A" 2 nil 1)
																		(york-swo--cell "B" 2 3 (plist-get request :title)))))
				 tasks)
		(dolist (phase phases)
			(setq row (+ row 2))
			(push (york-swo--row row nil
													 (york-swo--cell "A" row nil 2)
													 (york-swo--cell "B" row 1 (car phase)))
						rows)
			(setq row (1+ row))
			(push (york-swo--row row nil
													 (york-swo--cell "A" row nil "H")
													 (york-swo--cell "B" row 1 "Task Nbr")
													 (york-swo--cell "C" row 7 "Desc")
													 (york-swo--cell "D" row 1 "Target Date")
													 (york-swo--cell "E" row 1 "Date Completed")
													 (york-swo--cell "F" row 37 "Responsible")
													 (york-swo--cell "G" row 7 "Status/Notes")
													 (york-swo--cell "H" row 1 "Hours")
													 (york-swo--cell "I" row 1 "Minutes")
													 (york-swo--cell "J" row 1 "Total"))
						rows)
			(dolist (task (cdr phase))
				(setq row (1+ row))
				(let ((hours (plist-get task :hours))
							(number (plist-get task :number)))
					(push (york-swo--row
								 row nil
								 (york-swo--cell "A" row nil 3)
								 (york-swo--cell "B" row nil (and number (string-to-number number)))
								 (york-swo--cell "C" row 6 (plist-get task :description))
								 (york-swo--cell "D" row 2 (york-swo--date (plist-get task :target)))
								 (york-swo--cell "E" row 2 (york-swo--date (plist-get task :completed)))
								 (york-swo--cell "F" row 36 (plist-get task :responsible))
								 (york-swo--cell "G" row 6 (plist-get task :notes))
								 (york-swo--cell "H" row nil hours)
								 (york-swo--formula "I" row nil (format "H%d*60" row) (* hours 60))
								 (york-swo--formula "J" row nil (format "H%d*H$%d" row rate-row)
																		(* hours rate)))
								rows)
					(push (append task (list :row row)) tasks))))
		(let ((hours (apply #'+ (mapcar (lambda (task) (plist-get task :hours)) tasks))))
			(push (york-swo--row total-row nil
													 (york-swo--cell "G" total-row 8 "Total Hours")
													 (york-swo--formula "H" total-row 4
																							(format "SUM(H3:H%d)" last-task) hours))
						rows)
			(push (york-swo--row rate-row nil
													 (york-swo--cell "G" rate-row 8 "Hourly Rate")
													 (york-swo--cell "H" rate-row 4 rate))
						rows)
			(push (york-swo--row (1+ rate-row) nil
													 (york-swo--cell "G" (1+ rate-row) 8 "Total Amount")
													 (york-swo--formula "H" (1+ rate-row) 5
																							(format "H%d*H%d" total-row rate-row)
																							(* hours rate)))
						rows))
		(list (string-join (nreverse rows)) (1+ rate-row) rate-row (nreverse tasks))))

;;;; The service work order sheet

(defconst york-swo--estimate-note
	"This Service Work Order may contain costs that are estimates.  Because YORK’s actual time spent will be dependent upon client, trading partner, and/or other party’s co-operation and co-ordination, actual charges for estimates may vary.")

(defconst york-swo--email-note
	"Disclaimer:  York Worldwide Technologies, Inc. discourages the use of email for the purpose of delivery of production data in any solution.  Email may be used for notifications and summary reporting.")

(defun york-swo--order (request tasks rate-row)
	"Return the service work order sheet of REQUEST.
TASKS are the tasks with their rows in the project plan, and RATE-ROW
the row of its rate.  Tasks without hours are left out.  The value is
\(ROWS LAST-ROW MERGES)."
	(let* ((tasks (seq-filter (lambda (task) (> (plist-get task :hours) 0)) tasks))
				 (hours (apply #'+ (mapcar (lambda (task) (plist-get task :hours)) tasks)))
				 (row 6)
				 (total (+ 7 (length tasks)))
				 (plain "s=\"16\" customFormat=\"1\"")
				 (three (lambda (row attributes style &optional text)
									(york-swo--row row attributes
																 (york-swo--cell "A" row style text)
																 (york-swo--cell "B" row style)
																 (york-swo--cell "C" row style))))
				 (rows
					(list
					 (york-swo--row 1 "hidden=\"1\"" (york-swo--cell "A" 1 10))
					 (york-swo--row 2 "ht=\"129.75\" customHeight=\"1\""
													(york-swo--formula "A" 2 38 "'Project Plan'!B2"
																						 (plist-get request :title))
													(york-swo--cell "B" 2 38)
													(york-swo--cell "C" 2 38))
					 (york-swo--row 3 (concat plain " ht=\"26.25\"")
													(york-swo--cell "A" 3 13)
													(york-swo--cell "B" 3 14 "SERVICE WORK ORDER")
													(york-swo--cell "C" 3 15))
					 (york-swo--row 4 (concat plain " ht=\"26.25\" hidden=\"1\"")
													(york-swo--cell "A" 4 17)
													(york-swo--cell "C" 4 15))
					 (york-swo--row 5 (concat plain " ht=\"21\"")
													(york-swo--cell "A" 5 18
																					(concat "Client: " (plist-get request :client)))
													(york-swo--cell "B" 5 19)
													(york-swo--cell "C" 5 15))
					 (york-swo--row 6 plain
													(york-swo--cell "A" 6 20 "Timeline")
													(york-swo--cell "B" 6 21 "Activity Code / Description")
													(york-swo--cell "C" 6 20 "Hours")))))
		(unless tasks
			(user-error "No task of this request has billable hours"))
		(setq rows (nreverse rows))
		(dolist (task tasks)
			(setq row (1+ row))
			(let ((source (plist-get task :row)))
				(push (york-swo--row
							 row "s=\"24\" customFormat=\"1\""
							 (york-swo--cell "A" row 22 (plist-get task :timeline))
							 (york-swo--formula
								"B" row 23
								(format "IF(ISBLANK('Project Plan'!C%d),\"\",'Project Plan'!C%d)" source source)
								(plist-get task :description))
							 (york-swo--formula
								"C" row 22
								(format "IF(ISBLANK('Project Plan'!H%d),\"\",'Project Plan'!H%d)" source source)
								(plist-get task :hours)))
							rows)))
		(push (york-swo--row total (concat plain " ht=\"15.75\"")
												 (york-swo--cell "A" total 27)
												 (york-swo--cell "B" total 28 "Total Hours:")
												 (york-swo--formula "C" total 27 (format "SUM(C7:C%d)" (1- total)) hours))
					rows)
		;; Subtotal and discount are there for whoever needs them, but hidden.
		(seq-do-indexed
		 (lambda (label index)
			 (let ((row (+ total 1 index)))
				 (push (york-swo--row row (concat plain " ht=\"15.75\" hidden=\"1\"")
															(york-swo--cell "A" row 30)
															(york-swo--cell "B" row 31 label)
															(york-swo--cell "C" row (if label 32 30)))
							 rows)))
		 '("Subtotal:                  " "Discount:" nil))
		(setq row (+ total 4))
		(push (york-swo--row row (concat plain " ht=\"18.75\" customHeight=\"1\"")
												 (york-swo--cell "A" row 33)
												 (york-swo--cell "B" row 31 "Total:")
												 (york-swo--formula "C" row 32
																						(format "C%d*'Project Plan'!H%d" total rate-row)
																						(* hours (plist-get request :rate))))
					rows)
		(push (funcall three (+ total 5) (concat plain " ht=\"44.25\" customHeight=\"1\"")
									 44 york-swo--estimate-note)
					rows)
		(setq row (+ total 6))
		(push (york-swo--row row plain
												 (york-swo--cell "A" row 45 "This Estimate Good Through:")
												 (york-swo--cell "B" row 45)
												 (york-swo--formula "C" row 9 "TODAY()+30"
																						(+ 30 (york-swo--serial (calendar-current-date)))))
					rows)
		(setq row (+ total 7))
		(push (york-swo--row row (concat plain " ht=\"15\" customHeight=\"1\"")
												 (york-swo--cell "A" row 39 "I agree to the above and am requesting York to perform these services.")
												 (york-swo--cell "B" row 40)
												 (york-swo--cell "C" row 40))
					rows)
		(push (funcall three (+ total 8) plain 41
									 "Client: _________________________________________   Date: _____________________________")
					rows)
		(push (funcall three (+ total 9) "s=\"34\" customFormat=\"1\" ht=\"19.5\" customHeight=\"1\"" 43
									 "Please fax signed form to:  616-844-1577 or return via email to:  support@yorkwwt.com")
					rows)
		(push (funcall three (+ total 10) (concat plain " hidden=\"1\"") 42 york-swo--email-note)
					rows)
		(push (funcall three (+ total 11) (concat plain " ht=\"28.5\" customHeight=\"1\"") 42)
					rows)
		(list (string-join (nreverse rows))
					(+ total 11)
					;; The title, then the notes below the total, by their distance
					;; from the total hours row.
					(format "<mergeCells count=\"7\"><mergeCell ref=\"A2:C2\"/>%s</mergeCells>"
									(mapconcat (lambda (range)
															 (format "<mergeCell ref=\"A%d:%s%d\"/>"
																			 (+ total (nth 0 range)) (nth 1 range)
																			 (+ total (nth 2 range))))
														 '((5 "C" 5) (6 "B" 6) (7 "C" 7) (8 "C" 8) (9 "C" 9) (10 "C" 11))
														 "")))))

;;;; Writing the workbook

(defun york-swo--fill (file replacements)
	"Replace each (PLACEHOLDER . TEXT) of REPLACEMENTS in FILE."
	(let ((coding-system-for-read 'utf-8)
				(coding-system-for-write 'utf-8-unix))
		(with-temp-buffer
			(insert-file-contents file)
			(pcase-dolist (`(,placeholder . ,text) replacements)
				(goto-char (point-min))
				(search-forward placeholder)
				(replace-match text t t))
			(write-region nil nil file nil 'silent))))

(defun york-swo--file-name (title)
	"Return the name of the workbook of the request called TITLE."
	(concat (string-trim (replace-regexp-in-string
												" +" " "
												(replace-regexp-in-string "[][{}\\/:*?\"<>|]" "" title)))
					" Project Plan & SWO.xlsx"))

(defun york-swo--write (request file)
	"Write REQUEST, as returned by `york-swo--request', to the workbook FILE."
	(pcase-let* ((`(,plan-rows ,plan-last ,rate-row ,tasks) (york-swo--plan request))
							 (`(,order-rows ,order-last ,merges) (york-swo--order request tasks rate-row))
							 (work (make-temp-file "york-swo" t)))
		(unwind-protect
				(progn
					(copy-directory york-swo-template-directory work nil t t)
					(york-swo--fill (expand-file-name "xl/worksheets/sheet1.xml" work)
													`(("@DIMENSION@" . ,(format "A2:J%d" plan-last))
														("@ROWS@" . ,plan-rows)))
					(york-swo--fill (expand-file-name "xl/worksheets/sheet2.xml" work)
													`(("@DIMENSION@" . ,(format "A1:C%d" order-last))
														("@ROWS@" . ,order-rows)
														("@MERGES@" . ,merges)))
					(with-temp-buffer
						(let ((default-directory (file-name-as-directory work)))
							(unless (eq 0 (call-process york-swo-tar-program nil t nil
																					"--format" "zip" "-c" "-f" file
																					"[Content_Types].xml" "_rels" "docProps" "xl"))
								(error "tar: %s" (string-trim (buffer-string)))))))
			(delete-directory work t))))

(defun york-export-swo (&optional open)
	"Write the request around point to a project plan and SWO workbook.
The workbook goes to `york-workarea', replacing the one of an earlier
export.  With prefix argument OPEN, open it afterwards."
	(interactive "P")
	(let* ((request (york-swo--request))
				 (file (expand-file-name (york-swo--file-name (plist-get request :title))
																 york-workarea)))
		(york-swo--write request file)
		(when open
			(w32-shell-execute "open" file))
		(message "Wrote %s" file)))

(provide 'york-swo)
;;; york-swo.el ends here
