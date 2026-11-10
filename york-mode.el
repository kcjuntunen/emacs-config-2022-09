;;; york-mode.el --- a minor mode to contain functions for work -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Copyright (C) 2021 K. C. Juntunen

;; Author   : K. C. Juntunen <juntunen.kc@gmail.com>
;; URL      :
;; Package-Version:
;; Version  : 0.2
;; Keywords :

;; This file is NOT part of GNU Emacs.

;;; Commentary:


;;; Code:

;;;###autoload
(define-minor-mode york-mode
	"A container for handy, York-related functions."
	:lighter " ☥"
	:keymap (let ((map (make-sparse-keymap)))
						(define-key map (kbd "C-c y r") 'york-get-request-data)
						(define-key map (kbd "C-c y e") 'york-get-incident-data)
						(define-key map (kbd "C-c y q") 'york-insert-queue)
						(define-key map (kbd "C-c y Q") 'york-copy-queue-to-workarea)
						;; (define-key map (kbd "C-c y s") 'york-store-repo-name)
						;; (define-key map (kbd "C-c y g") 'york-open-local-repo-name)
						;; (define-key map (kbd "C-c y G") 'york-open-remote-repo-name)
						(define-key map (kbd "C-c y i") 'york-org-insert-last-screenshot)
						(define-key map (kbd "C-c y I") 'org-insert-window-screenshot)
						(define-key map (kbd "C-c y o") 'kc/rdp-open-from-org-property)
						map)
	(if york-mode
			(message "york-mode activated")
		(message "york-mode deactivated")))

(defcustom york-production-queue-path
	"//192.168.250.103/z-drive/ProductionQueues/"
	"Path to prduction queue root folder."
	:type 'string
	:group 'york)

(defcustom york-workarea
	"C:/fastrack/workarea/"
	"Path to workarea."
	:type 'string
	:group 'york)

(defcustom york-screenshot-searchpath
	org-directory
	"Where to search for screenshots to insert."
	:type 'string
	:group 'york)

(defcustom york-incident-looker-upper-path
	"C:/Users/k.c.juntunen/source/repos/Viewer.Etc/InsertIncident/bin/Release/net10.0/InsertIncident.exe"
	"The program that pulls in Incidents in Org format."
	:type 'string
	:group 'york)

(defcustom york-hang-looker-upper-path
	"C:/Users/k.c.juntunen/source/repos/Viewer.Etc/InsertMessedUpServerStates/bin/Release/net10.0/InsertMessedUpServerStates.exe"
	"The program that pulls in hung programs in Org format."
	:type 'string
	:group 'york)

(defcustom york-path-finder
	"C:/Users/k.c.juntunen/source/repos/Viewer.Etc/GetQPath/bin/Release/GetQPath.exe"
	"The program that find Queue paths."
	:type 'string
	:group 'york)

(defcustom york-remote-repo-path
	"G:/"
	"The location of remote git repos."
	:type 'string
	:group 'york)

(defcustom york-local-repo-path
	"D:/Source/Repos/"
	"The location of local git repos."
	:type 'string
	:group 'york)

(defcustom york--repo-name-property-name
	"Source"
	"The property under which we store and retrieve repo names."
	:type 'string
	:group 'york)
;;; Screenshot integration for Org-mode on Windows

(defcustom org-screenshot-save-directory "c:/Users/k.c.juntunen/OneDrive/org/2026"
  "Directory where window screenshots will be saved.
Should end with a slash. Will be created if it doesn't exist."
  :type 'directory
  :group 'org)

(defcustom org-screenshot-executable "c:/Users/k.c.juntunen/source/repos/Viewer.Etc/winshot/bin/Release/net8.0-windows10.0.26100.0/winshot.exe"
  "Full path to your C# window screenshot CLI executable."
  :type 'file
  :group 'org)

(defgroup york nil
	"Settings for `york-mode'."
	:prefix "york-"
	:group york-mode)

(defun york-store-repo-name (repo-name)
	"Store repo-name under the property name in `repo-name-property-name'."
	(interactive "sRepo Name: ")
	(org-entry-put (point) york--repo-name-property-name repo-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Request lookup: Project/Phase/Task data as an Org project plan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org-id)
(require 'seq)
(require 'subr-x)

;; The connection settings and the mysql --batch unescaping are shared
;; with package-config.d/22.allocations.el.
(defvar kc/allocations-db-host)
(defvar kc/allocations-database)
(defvar kc/allocations-auth-host)
(declare-function kc/allocations--unescape "22.allocations")
(declare-function kc/auth-get-key "13.auth-config")
(declare-function kc/auth-get-pwd "13.auth-config")

(defconst york--sql-datetime-regexp
	"\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?: [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)?\\'"
	"Matches a DATE or DATETIME value as the mysql client prints it.")

(defconst york--totals-formula
	(concat "#+TBLFM: @2$1=vsum((remote(%s,@I$%d..@>$%d)))::@2$3=$-2*$-1;f2"
					"::@2$4=remote(%s,@2$>);T"
					"::@2$5='(* (/ (string-to-number (org-table-time-string-to-seconds $-1)) 3600.0)"
					" (string-to-number $-3));f2\n")
	"Formula of the totals table; format args are table id, column (twice), table id.")

(defun york--sql (query)
	"Run QUERY with the mysql client.
Return the rows as alists of (COLUMN-NAME . VALUE); VALUE is a
string, or nil for NULL."
	(let ((user (kc/auth-get-key kc/allocations-auth-host :user))
				(pwd (kc/auth-get-pwd kc/allocations-auth-host)))
		(unless (and user pwd)
			(user-error "No %s entry in authinfo" kc/allocations-auth-host))
		(with-temp-buffer
			(let* ((coding-system-for-read 'utf-8)
						 (coding-system-for-write 'utf-8)
						 ;; MYSQL_PWD keeps the password off the command line.
						 (process-environment (cons (concat "MYSQL_PWD=" pwd)
																				process-environment))
						 (status (call-process "mysql" nil t nil
																	 "--batch" "--skip-ssl"
																	 "--default-character-set=utf8mb4"
																	 "--connect-timeout=15"
																	 "--host" kc/allocations-db-host
																	 "--user" user
																	 "--database" kc/allocations-database
																	 "--execute" query)))
				(unless (eq status 0)
					(error "mysql: %s" (string-trim (buffer-string))))
				(when-let* ((lines (split-string (buffer-string) "\n" t))
										(columns (split-string (car lines) "\t")))
					(mapcar (lambda (line)
										(seq-mapn #'cons columns
															(mapcar #'kc/allocations--unescape
																			(split-string line "\t"))))
									(cdr lines)))))))

(defun york--field (row column)
	"Return the value of COLUMN in ROW, a row from `york--sql'."
	(cdr (assoc column row)))

(defun york--org-stamp (value &optional date-only)
	"Format VALUE, a mysql DATE or DATETIME string, the way Org writes timestamps.
The brackets are left to the caller.  With DATE-ONLY, leave the time out."
	(pcase-let ((`(,sec ,min ,hour ,day ,month ,year) (parse-time-string value)))
		(format-time-string
		 (if date-only "%Y-%m-%d %a" "%Y-%m-%d %a %H:%M")
		 (encode-time (list (or sec 0) (or min 0) (or hour 0) day month year
												nil -1 nil)))))

(defun york--org-properties (row)
	"Return every column of ROW as Org property lines.
Columns named SomethingNbr are also written as Something."
	(concat
	 (format-time-string ":Captured: [%Y-%m-%d %a %H:%M]\n")
	 (mapconcat
		(pcase-lambda (`(,name . ,value))
			(cond
			 ((null value) (format ":%s: [null]\n" name))
			 ((string-blank-p value) (format ":%s: [empty]\n" name))
			 ((and (string-match-p york--sql-datetime-regexp value)
						 (not (string-prefix-p "0000" value)))
				(format ":%s: [%s]\n" name (york--org-stamp value)))
			 (t
				(concat (when (string-suffix-p "Nbr" name)
									(format ":%s: %s\n" (substring name 0 -3) value))
								(format ":%s: %s\n" name value)))))
		row "")))

(defun york--resolve-request-number (query)
	"Find the request number QUERY is about, or nil.
QUERY either contains a whole request number (R260703-3561) or just
its last four digits, which stand for the newest request having them."
	(let ((case-fold-search nil))
		(if (string-match "R[0-9]\\{6\\}-[0-9]\\{4\\}" query)
				(match-string 0 query)
			(when-let* ((digits (seq-find (lambda (s)
																			(string-match-p "\\`[0-9]\\{4\\}\\'" s))
																		(split-string query "[^0-9R]"))))
				(york--field
				 (car (york--sql
							 (format "SELECT RequestNbr FROM clientrequests
WHERE RequestNbr LIKE '%%%s%%' ORDER BY RequestDate DESC LIMIT 1"
											 digits)))
				 "RequestNbr")))))

(defun york--insert-totals (table-id column rate)
	"Insert the totals table for the columnview named TABLE-ID.
COLUMN is the columnview column holding the estimated hours, RATE the
project's billable rate."
	(insert "\n\\\\\n\n"
					"#+caption: Totals\n"
					"| Total est. Hours | Rate | Total est. Cost | Total act. Hours | Total act. Cost |\n"
					"|-\n"
					(format "| | %.2f | | | |\n" rate)
					(format york--totals-formula table-id column column table-id)))

(defun york--task-keyword (task notes phase request)
	"Return the TODO keyword for TASK.
NOTES are its status rows; PHASE and REQUEST the rows it belongs to."
	(let ((complete (string-prefix-p "Y" (or (york--field task "IsTaskCompletedInd") ""))))
		(cond
		 ;; Marked complete without anything logged against it.
		 ((and complete (null notes)) "TODO")
		 (complete "DONE")
		 ((null notes) "TODO")
		 ((and (string-search "Exam" (or (york--field phase "Description") ""))
					 (string-search "Surgery" (or (york--field request "Status") "")))
			"DONE")
		 (t "WIP"))))

(defun york--insert-logbook (notes)
	"Insert NOTES, the status rows of a task, as a LOGBOOK drawer."
	(when notes
		(insert ":LOGBOOK:\n")
		(dolist (note notes)
			(let* ((start (york--field note "StatusTime"))
						 (minutes (string-to-number (or (york--field note "BillableMinutes") "0")))
						 (end (format-time-string
									 "%Y-%m-%d %H:%M:%S"
									 (time-add (date-to-time start) (* 60 minutes)))))
				(insert (format "- Note taken on [%s] \\\\\n" (york--org-stamp start))
								(format "  %s -- %s\n"
												(or (york--field note "Description") "")
												(or (york--field note "StaffMember") ""))
								(format "CLOCK: [%s]--[%s] => %02d:%02d\n\n"
												(york--org-stamp start) (york--org-stamp end)
												(/ minutes 60) (% minutes 60)))))
		(insert ":END:\n")))

(defun york--insert-allocation (allocation rate)
	"Insert the details of ALLOCATION, a resource_allocations row.
RATE is the project's billable rate."
	(when allocation
		(let ((hours (/ (string-to-number (york--field allocation "AllocatedMinutes"))
										60.0)))
			(insert (format-time-string
							 "\n**** Allocation Details as of [%Y-%m-%d %a] :noexport:\n")
							(format "- Resource: %s <%s>\n"
											(or (york--field allocation "Name") "")
											(or (york--field allocation "Email") ""))
							(format "  - Default Rate: $%.2f\n"
											(string-to-number
											 (or (york--field allocation "DefaultHourlyRate") "0")))
							(format "- Allocation: %.2f hours @ $%.2f ($%.2f)\n"
											hours rate (* hours rate))
							(format "- Allocation Date: [%s]\n"
											(york--org-stamp
											 (york--field allocation "AllocationDateTime") t))))))

(defun york--request-org (query)
	"Return the project plan of the request QUERY is about, as Org text."
	(let* ((number (or (york--resolve-request-number query)
										 (user-error "Couldn't find anything for %s" query)))
				 (where (format "RequestNbr='%s'" number))
				 (request (or (car (york--sql (format "SELECT * FROM clientrequests WHERE %s"
																							where)))
											(user-error "Couldn't find anything for %s" query)))
				 (project (car (york--sql (format "SELECT BillableRate, Status FROM projects WHERE %s"
																					where))))
				 (phases (york--sql (format "SELECT * FROM phases WHERE %s
ORDER BY CAST(PhaseNbr AS SIGNED)"
																		where)))
				 (tasks (york--sql (format "SELECT * FROM tasks WHERE %s
ORDER BY CAST(TaskNbr AS SIGNED)"
																	 where)))
				 (notes (york--sql (format "SELECT PhaseNbr, TaskNbr, StatusTime, Description,
StaffMember, BillableMinutes FROM status WHERE %s ORDER BY StatusTime DESC"
																	 where)))
				 ;; Reversed, so that the last allocation of a task is found first.
				 (allocations
					(reverse
					 (york--sql (format "SELECT ra.PhaseNbr, ra.TaskNbr, ra.AllocatedMinutes,
ra.AllocationDateTime, rm.Name, rm.Email, rm.DefaultHourlyRate
FROM resource_allocations ra
LEFT JOIN resource_master rm ON rm.Key1 = ra.ResourceKey
WHERE ra.RequestNbr='%s' ORDER BY ra.ResourceKey, ra.AllocationDateTime"
															number))))
				 (number (york--field request "RequestNbr"))
				 (description (or (york--field request "Description") ""))
				 (rate (string-to-number (or (york--field project "BillableRate") "0")))
				 (deadline (and (not (member (york--field request "ExternalDeadlineInd")
																		 '(nil "" "N")))
												(york--org-stamp (york--field request "ExternalDeadline") t)))
				 (same-task (lambda (phase-nbr task-nbr)
											(lambda (row)
												(and (equal (york--field row "PhaseNbr") phase-nbr)
														 (equal (york--field row "TaskNbr") task-nbr)))))
				 (workarea york-workarea)
				 (table-id (format "Y%sF" number)))
		(with-temp-buffer
			;; The request.
			(insert (format "* %s {%s}\n" number description))
			(when deadline
				(insert (format "DEADLINE: <%s>\n" deadline)))
			(insert ":PROPERTIES:\n"
							(format ":ID: %s\n" (org-id-uuid)))
			(when deadline
				(insert (format ":ExternalDeadline: <%s>\n" deadline)))
			(insert (format ":Query: \"%s\"\n" query)
							(york--org-properties request)
							(format ":Task: %s %s\n" number description)
							":TypeKey: 1\n"
							(format ":ProjectStatus: %s\n" (or (york--field project "Status") ""))
							(format ":EXPORT_FILE_NAME: %sDesign - %s {%s}\n"
											workarea number description)
							(format ":TABLE_EXPORT_FILE: %s%s.yorkformat.csv\n" workarea number)
							":TABLE_EXPORT_FORMAT: orgtbl-to-csv\n"
							":COLUMNS:  %TypeKey(H) %Task(Desc/Task ID) %Item(Desc) %targetDate"
							" %dateCompleted %Responsible %status(Status/Notes)"
							" %BillableHours(Hours) %clocksum(Actual)\n"
							":END:\n\n"
							(format "#+name: %s\n" table-id)
							"#+begin: columnview :maxlevel 3\n"
							"#+caption: York Format Project Plan /(not for human consumption)/\n"
							"#+end:\n")
			(york--insert-totals table-id 8 rate)
			(dolist (phase phases)
				(let* ((phase-nbr (york--field phase "PhaseNbr"))
							 (phase-description (or (york--field phase "Description") ""))
							 (table-id (format "%sRP%s" number phase-nbr)))
					;; The phase.
					(insert (format "** %s [%%]\n" phase-description)
									":PROPERTIES:\n"
									(format ":ID: %s\n" (org-id-uuid))
									(york--org-properties phase)
									(format ":Task: Phase: %s - %s\n" phase-nbr phase-description)
									":TaskName: Exam/Design\n"
									":COLUMNS:  %Timeline %todo(Status) %Item"
									" %BillableHours(Billable Hours) %clocksum(Actual)\n"
									":TypeKey: 2\n"
									":END:\n\n"
									(format "#+name: %s\n" table-id)
									"#+begin: columnview :skip-empty-rows t\n"
									(format "#+caption: Phase %s - %s\n" phase-nbr phase-description)
									"#+end:\n")
					(york--insert-totals table-id 4 rate)
					;; Its tasks.
					(dolist (task (seq-filter (lambda (row)
																			(equal (york--field row "PhaseNbr") phase-nbr))
																		tasks))
						(let* ((mine (funcall same-task phase-nbr (york--field task "TaskNbr")))
									 (task-notes (seq-filter mine notes))
									 (task-description (or (york--field task "Description") ""))
									 (minutes (string-to-number
														 (or (york--field task "EstimatedMinutes") "0"))))
							(insert (format "*** %s %s\n"
															(york--task-keyword task task-notes phase request)
															task-description))
							(when (string-prefix-p "Y" (or (york--field task "IsTaskCompletedInd") ""))
								(insert (format "CLOSED: [%s]\n"
																(york--org-stamp
																 (york--field task "TaskCompletionDate")))))
							(insert ":PROPERTIES:\n"
											(format ":ID: %s\n" (org-id-uuid))
											(york--org-properties task)
											(format ":Effort: %d:%02d\n" (/ minutes 60) (% minutes 60))
											(format ":BillableHours: %.1f\n" (/ minutes 60.0))
											(format ":TaskName: %s\n" task-description)
											":Responsible: Tier 2\n"
											":TypeKey: 3\n"
											":END:\n")
							(york--insert-logbook task-notes)
							(york--insert-allocation (seq-find mine allocations) rate)))))
			(buffer-string))))

(defun york-get-request-data (request-number)
	"Insert Project/Phase/Task data into buffer in Org format.
REQUEST-NUMBER is a request number, or the last four digits of one."
	(interactive "sRequestNbr: ")
	(insert (york--request-org request-number)))

(defun york-get-incident-data (incident-number)
	"Insert incident data into buffer in Org format."
	(interactive "sIncident Number: ")
	(insert (shell-command-to-string
					 (format "%s %s"
									 york-incident-looker-upper-path incident-number))))

(defun york-get-hang-data ()
	"Insert incident data into buffer in Org format."
	(interactive)
	(insert (shell-command-to-string york-hang-looker-upper-path)))

(defun york--get-inputq-path ()
	"Get the path to production input queues"
	(concat york-production-queue-path "incomeq/"))

(defun york--get-processq-path ()
	"Get the path to production process queues"
	(concat york-production-queue-path "processq/"))

(defun york--get-reprocessq-path ()
	"Get the path to production reprocess queues"
	(concat "//192.168.250.224/M-drive/" "reprocessq/"))

(defun york--get-outputq-path ()
	"Get the path to production output queues"
	(concat "//192.168.250.225/productionQueues/" "outputq/"))

(defun york--get-arcoutq-path ()
	"Get the path to production output queues"
	(concat "//192.168.250.225/productionQueues/" "Arcoutq/"))

;; (defun york--get-queue-path (queuekey)
;; 	"Convert queue key to queue path."
;; 	(let* ((queue-type (substring queuekey 0 1))
;; 				 (qpath (cond ((string-equal queue-type "I") (york--get-inputq-path))
;; 											((string-equal queue-type "P") (york--get-processq-path))
;; 											((string-equal queue-type "R") (york--get-reprocessq-path))
;; 											((string-equal queue-type "O") (york--get-outputq-path))
;; 											((string-equal queue-type "A") (york--get-arcoutq-path)))))
;; 		(concat qpath queuekey ".dat")))

(defun york--get-queue-path (queuekey)
	"Convert queue key to queue path."
	(string-trim (shell-command-to-string (format "%s %s" york-path-finder queuekey))))

(defun york--get-local-repo-name ()
	"Get the local repo for the associated code."
	(let ((repo-name (org-entry-get (point) york--repo-name-property-name t)))
		(concat york-local-repo-path repo-name "/" repo-name ".sln")))

(defun york--get-remote-repo-name ()
	"Get the remote repo for the associated code."
	(let ((repo-name (org-entry-get (point) york--repo-name-property-name t)))
		(concat york-remote-repo-path repo-name ".git")))

(defun york-open-local-repo-name ()
	"Open thing in the property named `york--repo-name-property-name'."
	(interactive)
	(let ((thing-to-open (york--get-local-repo-name)))
		(if (not (file-exists-p thing-to-open))
				(error (format "File `%s' not found" thing-to-open))
			(org-open-file thing-to-open nil))))

(defun york-open-remote-repo-name ()
	"Open thing in the property named `york--repo-name-property-name'."
	(interactive)
	(let ((here default-directory)
				(thing-to-open (york--get-remote-repo-name)))
		(if (not (file-exists-p thing-to-open))
				(error (format "Directory `%s' not found" thing-to-open))
			(save-excursion
				(cd thing-to-open)
				(magit-status-setup-buffer here)))
		(cd here)))

(defun york-find-queue-directly (queuekey)
	"Insert queue contents into current buffer at point."
	(interactive "sQueue Key: ")
	(let* ((file-to-insert (york--get-queue-path queuekey))
				 (fa (file-attributes file-to-insert)))
		(if (not (file-exists-p file-to-insert))
				(message "Couldn't find %s" file-to-insert)
			(if (not (y-or-n-p
								(format "Really open %s file? "
												(file-size-human-readable
												 (file-attribute-size fa)))))
					(message "Not opening %s" queuekey)
				(find-file file-to-insert)))))


(defun york-copy-queue-to-workarea (queuekey)
	"Copy payload indicated by QUEUEKEY to workarea."
	(interactive "sQueue Key: ")
	(let* ((file-to-copy (york--get-queue-path queuekey))
				 (destination (concat york-workarea queuekey ".dat")))
		(cond
		 ((not (file-exists-p file-to-copy))
			(message "Couldn't find %s" file-to-copy))
		 ((file-exists-p destination)
			(message "%s already exists" destination))
		 ((not (y-or-n-p
						(format "Really copy %s file to %s? "
										(file-size-human-readable
										 (file-attribute-size (file-attributes file-to-copy)))
										destination)))
			(message "Not copying %s" queuekey))
		 (t
			(message "Copying %s to %s" file-to-copy destination)
			(copy-file file-to-copy destination)))))

(defun york-insert-queue (queuekey)
	"Insert queue contents into current buffer at point."
	(interactive "sQueue Key: ")
	(let* ((file-to-insert (york--get-queue-path queuekey))
				 (fa (file-attributes file-to-insert))
				 (temp (generate-new-buffer " *queue-temp*")))
		(if (not (file-exists-p file-to-insert))
				(message "Couldn't find %s" file-to-insert)
			(if (not (y-or-n-p
								(format "Really insert %s file into %s? "
												(file-size-human-readable
												 (file-attribute-size fa))
												(current-buffer))))
					(message "Not inserting")
				(message "Inserting %s in %s" file-to-insert (buffer-name))
				(with-current-buffer temp
					(insert (format "#+caption: %s\n#+begin_src text :tangle \"%s\"\n" file-to-insert
													(concat york-workarea (file-name-nondirectory file-to-insert))))
					(goto-char (point-max))
					(insert-file-contents file-to-insert)
					(goto-char (point-max))
					(insert "\n#+end_src"))
				(insert-buffer-substring temp)))))

(defun york-copy-queue-to-workarea-and-open (queuekey)
	"Copy payload indicated by QUEUEKEY to workarea."
	(interactive "sQueue Key: ")
	(york-copy-queue-to-workarea queuekey)
	(find-file (concat york-workarea queuekey ".dat")))

(defun york-copy-queues-to-workarea (queue-keys)
	(cl-loop for queue-key in queue-keys
					 do (york-copy-queue-to-workarea queue-key)))

(defun york-org-insert-last-screenshot ()
	"After saving a screenshot insert it as an inline image."
	(interactive)
	(let ((most-recent-screenshot-path
				 (car (directory-files york-screenshot-searchpath t "^Screenshot.*png$" t))))
		(insert (format "#+caption: %s\n[[file:%s]]"
										(file-name-nondirectory most-recent-screenshot-path) most-recent-screenshot-path))))

(defun kc/kill-buffers-commonly-in-my-way ()
	"Sometimes I have a bunch of buffers open which I don't need anymore but are
getting in the way of selecting the buffers I do want."
	(interactive)
	(let ((cnt 0))
		(dolist (b (buffer-list))
			(if (string-match-p "[Ll]og\\|repo" (buffer-name b))
					(progn (kill-buffer b)
								 (setq cnt (+ 1 cnt)))))
		(message "Killed %s buffer(s)" cnt)))

(defun kc/hostname-at-point ()
  "Return a hostname-like word at point, or nil if none.
Allowed characters: letters, digits, dot, hyphen."
  (let ((chars "A-Za-z0-9.-"))
    (save-excursion
      (skip-chars-backward chars)
      (let ((start (point)))
        (skip-chars-forward chars)
        (let ((end (point)))
          (when (< start end)
            (buffer-substring-no-properties start end)))))))

(defun kc/rdp-prompt-for-server ()
  "Prompt for a server name using completion."
  (completing-read "RDP server: " kc/rdp-server-list nil nil))

(defun kc/rdp-open (server)
  "Start a Remote Desktop session to SERVER as a non-blocking process."
  (let ((cmd (list "c:/windows/system32/mstsc.exe" (concat "/v:" server))))
    (apply #'start-process "rdp-session" nil cmd)))

(defun kc/rdp-open-at-point ()
  "Start a Remote Desktop session using the word at point as the server name."
  (interactive)
  (let ((server (kc/hostname-at-point)))
    (unless server
      (setq server (kc/rdp-prompt-for-server)))
    (kc/rdp-open server)))

(defun kc/rdp-open-from-org-property (&optional property)
  "Start a Remote Desktop session using PROPERTY (default: SERVER) from the
nearest Org property drawer."
  (interactive)
  (let* ((prop (or property "SERVER"))
				 (value (org-entry-get (point) prop t)))
    (unless value
      (setq value (kc/rdp-prompt-for-server))
			(org-set-property prop value))
    (kc/rdp-open value)))

(defun org-insert-window-screenshot ()
  "Run the window screenshotter and insert its Org-ready output directly.
The executable is expected to print lines like:
#+caption: Window Title Here
[[file:filename.png]]"
  (interactive)
  (let* ((save-dir (expand-file-name org-screenshot-save-directory))
         (exe-path (expand-file-name org-screenshot-executable))
         (origin-buffer (current-buffer))
         (origin-point (point))
         (output-buffer (generate-new-buffer " *screenshot-output*"))
         process)

    (unless (file-exists-p exe-path)
      (error "Screenshot executable not found: %s" exe-path))

    (unless (file-directory-p save-dir)
      (make-directory save-dir t))

    (message "Screenshot tool launched. Click a window to capture...")

    (setq process (start-process "window-screenshotter" output-buffer exe-path save-dir))

    (set-process-sentinel
     process
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (with-current-buffer (process-buffer proc)
           (let ((output (buffer-string)))
             (with-current-buffer origin-buffer
               (save-excursion
                 (goto-char origin-point)
                 ;; Add spacing if needed
                 (unless (bobp)
                   (insert "\n\n"))
                 (insert (string-trim output))
                 (insert "\n"))
               ;; Refresh images in case the link was inserted
               (org-redisplay-inline-images))
             (message "Screenshot and caption inserted.")))
         (kill-buffer output-buffer))))))

(defun kc/find-workarea-projects ()
	"Find VS solutions cloned to my fastrack directory."
	(interactive)
	(kc/find-dired-named "c:/fastrack/" "-t f -i -g \"*sln*\"" "*Fastrack Projects*"))

(defun kc/find-dired-named (dir args name)
	"Run fd in DIR with ARGS, show the results in a Dired buffer called NAME.
ARGS are fd arguments, not find ones.  -u (search hidden and ignored
files, like find) and -l (ls-style listing, which Dired needs) are
added.  If the buffer already exists it is just shown again."
	(interactive)
	(let ((buf (get-buffer name)))
		(if buf
				(switch-to-buffer buf)
			(find-dired-with-command dir (format "fd -u -l %s" args))
			(let ((buf (get-buffer "*Find*")))
				(rename-buffer name)
				(switch-to-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Daily workarea rotation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require (quote dired))

(defun york--start-of-today ()
	"Return the time value for local midnight today."
	(let ((now (decode-time)))
		(encode-time (list 0 0 0
											 (decoded-time-day now)
											 (decoded-time-month now)
											 (decoded-time-year now)
											 nil -1 nil))))

(defun york-rotate-workarea (&optional no-confirm)
	"Move everything in `york-workarea' last modified before today into
a sibling folder named after today's date (e.g. C:/fastrack/2026-11-10).
Files and folders touched today stay put.  If today's folder already
exists it is reused; anything that would collide with an existing name
is skipped and reported.  With prefix arg NO-CONFIRM, don't ask."
	(interactive "P")
	(let* ((workarea (file-name-as-directory (expand-file-name york-workarea)))
				 (parent   (file-name-directory (directory-file-name workarea)))
				 (target   (expand-file-name (format-time-string "%Y-%m-%d") parent))
				 (cutoff   (york--start-of-today))
				 (old      (seq-filter
										(lambda (f)
											(time-less-p (file-attribute-modification-time
																		(file-attributes f))
																	 cutoff))
										(directory-files workarea t directory-files-no-dot-files-regexp)))
				 (moved 0)
				 (skipped nil))
		(cond
		 ((null old)
			(message "Workarea: nothing older than today."))
		 ((or no-confirm
					(y-or-n-p (format "Move %d item(s) from %s to %s? "
														(length old) workarea target)))
			(make-directory target t)
			(dolist (f old)
				(let ((dest (expand-file-name (file-name-nondirectory f) target)))
					(if (file-exists-p dest)
							(push (file-name-nondirectory f) skipped)
						(condition-case err
								(progn (rename-file f dest)
											 (setq moved (1+ moved)))
							;; e.g. a file still open in Excel
							(file-error (push (format "%s [%s]" (file-name-nondirectory f)
																		(error-message-string err))
															skipped))))))
			;; Refresh any Dired buffers showing either folder.
			(dolist (dir (list workarea target))
				(when-let ((buf (dired-find-buffer-nocreate dir)))
					(with-current-buffer buf (revert-buffer))))
			(message "Workarea: moved %d item(s) to %s%s"
							 moved target
							 (if skipped
									 (format "; skipped %d: %s"
													 (length skipped)
													 (string-join (nreverse skipped) ", "))
								 ""))))))

(global-set-key (kbd "C-c y w") #'york-rotate-workarea)

(global-set-key (kbd "C-c y p") #'kc/rdp-open-at-point)
(global-set-key (kbd "C-c y m") #'kc/extract-unique-matches)
(global-set-key (kbd "C-c y s") #'kc/find-workarea-projects)


(load-file "~/.emacs.d/x12-mode/x12-mode.el")
(add-to-list 'auto-mode-alist '("\\.x12\\'" . x12-mode))
(add-to-list 'auto-mode-alist '("\\.edi\\'" . x12-mode))
(add-to-list 'auto-mode-alist '("\\.ansi\\'" . x12-mode))

;;;###autoload
(add-hook 'org-mode-hook 'york-mode)

(message "york-mode loaded")
(provide 'york-mode)
