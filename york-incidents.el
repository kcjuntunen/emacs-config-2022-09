;;; york-incidents.el --- Incidents and system snapshots as Org  -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Copyright (C) 2026  K. C. Juntunen

;; Author: K. C. Juntunen <k.c.juntunen@yorkwwt.com>
;; Keywords: data, outlines

;;; Commentary:

;; Two commands that read the System and Foundation databases (see
;; package-config.d/21.york-db.el) and insert what they find as Org:
;;
;; `york-get-incident-data' inserts incidents, given their numbers.
;;
;; `york-get-hang-data' inserts a system snapshot: the processes that
;; haven't reported for a while, the Foundation locks that might have
;; to do with it, and the recent incidents of the servers concerned.
;;
;; Loaded by york-mode.el, whose `york--org-stamp' this uses.

;;; Code:

(require 'org)
(require 'org-src)
(require 'seq)
(require 'subr-x)

(defvar york-workarea)
(defvar york--sql-datetime-regexp)
(declare-function york--org-stamp "york-mode")
(declare-function york-db-query "21.york-db")
(declare-function york-db-field "21.york-db")
(declare-function york-db-quote "21.york-db")

(defconst york--session-key-regexp
	".*\\([0-9]\\{14\\}-[0-9]\\{1,5\\}-[Aa][Mm][Xx]-[IiPp][A-Za-z0-9]\\{7\\}-[A-Za-z0-9]+\\)"
	"Matches a line mentioning a session stats key; group 1 is the key.")

(defconst york--lock-table-header
	"| server | comment | date added | thread id | version | key1 | minutes since |\n|----\n"
	"Header of the Foundation lock tables.")

(defconst york--stalled-query
	"SELECT t.*, TIMESTAMPDIFF(SECOND, t.updated, NOW()) / 60.0 AS minutes FROM (
SELECT SUBSTRING_INDEX(ServerName, '-', 1) `server`,
  CONCAT(SUBSTRING_INDEX(ServerName, '-', -1), ' ', CurrentStatus) `description`,
  LastUpdatedDate `updated`
FROM serverstatus
WHERE LastUpdatedDate < (NOW() - INTERVAL %1$d MINUTE)
UNION
SELECT LastServer, CONCAT('Task ', tt.Key1, ': ', Description), LastRan
FROM tasktypes tt
WHERE ActiveInd='Y' AND LastRan < (NOW() - INTERVAL %1$d MINUTE)
UNION
SELECT SUBSTRING_INDEX(ServerName, '-', 1),
  CONCAT(Mailbox, ' ', SUBSTRING_INDEX(ServerName, '-', -1), ' ', CurrentStatus),
  LastUpdatedDate
FROM applicationstatus
WHERE LastUpdatedDate < (NOW() - INTERVAL %1$d MINUTE)
) t ORDER BY server, description"
	"Finds the processes that haven't reported lately; format arg is the minutes.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shared pieces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun york--now-stamp ()
	"Return the current time as an inactive Org timestamp."
	(format-time-string "[%Y-%m-%d %a %H:%M]"))

(defun york--file-stamp ()
	"Return the current time the way the export file names have it."
	(format-time-string "%Y-%m-%d %H-%M"))

(defun york--session-key (text)
	"Return the session stats key mentioned in TEXT, or nil."
	(let ((case-fold-search nil))
		(and text
				 (string-match york--session-key-regexp text)
				 (match-string 1 text))))

(defun york--insert-break ()
	"Insert an Org line break, with a blank line on either side."
	(insert "\n\\\\\n\n"))

(defun york--insert-block (caption language text)
	"Insert TEXT as a captioned source block in LANGUAGE, after a break."
	(york--insert-break)
	(insert (format "#+caption: %s\n#+begin_src %s\n" caption language)
					(org-escape-code-in-string (or text ""))
					"\n#+end_src\n"))

(defun york--log-time (value)
	"Format VALUE, a mysql DATETIME string, with milliseconds."
	(if (and value
					 (string-match "\\`\\([-0-9]+ [0-9:]+\\)\\(?:\\.\\([0-9]+\\)\\)?" value))
			(format "%s.%s"
							(match-string 1 value)
							(substring (concat (match-string 2 value) "000") 0 3))
		(or value "")))

(defun york--session-stats (key)
	"Return the notes of the session KEY as log lines.
A note that doesn't start with its own timestamp gets the time it was
added."
	(mapconcat
	 (lambda (row)
		 (let ((note (york-db-field row "Note")))
			 (if (string-match-p "\\[[0-9]\\{4\\}" note)
					 (concat "  " note)
				 (format "  [%s] %s"
								 (york--log-time (york-db-field row "DateAdded"))
								 note))))
	 (seq-filter (lambda (row) (york-db-field row "Note"))
							 (york-db-query
								(format "SELECT * FROM sessionstats WHERE sessionId = %s"
												(york-db-quote key))
								'system t))
	 "\n"))

(defun york--property-value (value)
	"Return VALUE, a database value, fit to be an Org property."
	(cond
	 ((null value) "")
	 ((string-match-p york--sql-datetime-regexp value)
		(format "[%s]" (york--org-stamp value)))
	 (t (string-replace "\n" " " value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incidents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun york--incident-number (incident)
	"Return the number of INCIDENT, a row of incidents or extincidents."
	(or (york-db-field incident "IncidentNumber")
			(york-db-field incident "IncidentNbr")))

(defun york--incident-lines (incident)
	"Return the non-empty lines of the specific info of INCIDENT."
	(split-string (or (york-db-field incident "SpecificInfo") "") "\n" t))

(defun york--incident-session-key (incident)
	"Return the session stats key INCIDENT mentions, or nil.
The additional info is looked at first, then the specific info from
the last line up, then the other columns."
	(or (york--session-key (york-db-field incident "AdditionalInfo"))
			(seq-some #'york--session-key (reverse (york--incident-lines incident)))
			(seq-some (lambda (column) (york--session-key (cdr column)))
								(reverse incident))))

(defun york--insert-incident-properties (incident &optional query)
	"Insert the columns of INCIDENT as a property drawer.
The info columns are left out, and values of more than one line
follow the drawer as blocks.  QUERY, if given, goes in as well."
	(let ((number (york--incident-number incident))
				(long nil))
		(insert ":PROPERTIES:\n"
						(format-time-string ":Captured: [%Y-%m-%d %a %H:%M]\n"))
		(when number
			(insert (format ":EXPORT_FILE_NAME: %sIncident %s %s\n"
											york-workarea number (york--file-stamp))))
		(when query
			(insert (format ":Query: %s\n" query)))
		(pcase-dolist (`(,name . ,value) incident)
			(cond
			 ((string-match-p "info" (downcase name)))
			 ((null value) (insert (format ":%s: nil\n" name)))
			 ((string-blank-p value) (insert (format ":%s: [empty]\n" name)))
			 ((string-search "\n" value) (push (cons name value) long))
			 (t
				;; The names the rest of my notes use for these.
				(when (and (string-search "Account" name)
									 (not (string= name "ClientAccount")))
					(insert (format ":ClientAccount: %s\n" value)))
				(when (and (string-search "TradingPartner" name)
									 (not (string= name "TradingPartnerAccount")))
					(insert (format ":TradingPartnerAccount: %s\n" value)))
				(insert (format ":%s: %s\n" name (york--property-value value))))))
		(insert ":END:\n")
		(pcase-dolist (`(,name . ,value) (nreverse long))
			(york--insert-block name "text" value))))

(defun york--insert-incident (incident level &optional query)
	"Insert INCIDENT as an Org entry at LEVEL.
QUERY, if given, is recorded in its properties."
	(let ((lines (york--incident-lines incident))
				(key (york--incident-session-key incident)))
		(insert (format "%s Incident # %s: %s [%s]\n"
										(make-string level ?*)
										(york--incident-number incident)
										(or (car lines) "")
										(york--org-stamp (york-db-field incident "DateAdded"))))
		(york--insert-incident-properties incident query)
		(dolist (line (cdr lines))
			(insert line "\n"))
		(york--insert-block "Additional Info" "text"
												(york-db-field incident "AdditionalInfo"))
		(when key
			(york--insert-block (format "Session Stats Key: %s" key) "log-view"
													(york--session-stats key)))))

(defun york--incidents-org (query)
	"Return the incidents whose numbers are in QUERY, as Org text."
	(let* ((words (split-string query "[ \t\r\n,]+" t))
				 (numbers (seq-filter (lambda (word) (string-match-p "\\`[0-9]+\\'" word))
															words))
				 (query (string-join words " ")))
		(unless numbers
			(user-error "Please provide one or more incident numbers"))
		(let* ((in (string-join numbers ","))
					 (incidents (york-db-query
											 (format "SELECT * FROM incidents WHERE IncidentNumber IN (%s)" in)
											 'system t))
					 (ext-incidents (york-db-query
													 (format "SELECT * FROM extincidents WHERE IncidentNbr IN (%s)" in)
													 'system t))
					 (all (append incidents ext-incidents))
					 (several (cdr all)))
			(unless all
				(user-error "No incidents found for %s" query))
			(with-temp-buffer
				(when several
					;; A parent entry with a summary of them all.
					(insert (format "* %d %s and %d %s %s\n"
													(length incidents)
													(if (= (length incidents) 1) "incident" "incidents")
													(length ext-incidents)
													(if (= (length ext-incidents) 1) "ext-incident" "ext-incidents")
													(york--now-stamp))
									":PROPERTIES:\n"
									(format ":Query: %s\n" query)
									(format ":EXPORT_FILE_NAME: %sIncidents %s\n"
													york-workarea (york--file-stamp))
									":END:\n")
					(york--insert-break)
					(insert "#+caption: Incident Summary\n"
									"|id|server|message|timestamp|\n"
									"|---\n")
					(dolist (incident all)
						(insert (format "|%s|%s|%s|[%s]|\n"
														(york--incident-number incident)
														(or (york-db-field incident "Server") "")
														(or (car (york--incident-lines incident)) "")
														(york--org-stamp (york-db-field incident "DateAdded")))))
					(york--insert-break))
				(dolist (incident all)
					(york--insert-incident incident (if several 2 1)
																 (unless several query)))
				(buffer-string)))))

(defun york-get-incident-data (incident-numbers)
	"Insert incident data into buffer in Org format.
INCIDENT-NUMBERS is one or more incident numbers, separated by spaces,
commas or line breaks."
	(interactive "sIncident Number(s): ")
	(insert (york--incidents-org incident-numbers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System snapshot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun york--us-date-and-time (value)
	"Return VALUE, a mysql DATETIME string, as (\"11/9/2026\" . \"6:30 PM\")."
	(if (and value
					 (string-match
						"\\`\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\)" value))
			(let ((hour (string-to-number (match-string 4 value))))
				(cons (format "%d/%d/%s"
											(string-to-number (match-string 2 value))
											(string-to-number (match-string 3 value))
											(match-string 1 value))
							(format "%d:%s %s"
											(if (zerop (% hour 12)) 12 (% hour 12))
											(match-string 5 value)
											(if (< hour 12) "AM" "PM"))))
		(cons (or value "") "")))

(defun york--day-description ()
	"Return what part of which day it is, like \"Tuesday morning\"."
	(format-time-string
	 (if (>= (decoded-time-hour (decode-time)) 12) "%A night" "%A morning")))

(defun york--minutes-since (row column)
	"Return the minutes in COLUMN of ROW as a number."
	(string-to-number (or (york-db-field row column) "0")))

(defun york--insert-locks (locks caption)
	"Insert LOCKS, rows of lockstatus, as a table with CAPTION."
	(york--insert-break)
	(insert (format "#+caption: %s\n" caption)
					york--lock-table-header)
	(dolist (lock locks)
		(insert (format "|%s|%s|[%s]|%s|%s|%s|%.1f|\n"
										(or (york-db-field lock "Server") "")
										(or (york-db-field lock "Comment") "")
										(york--org-stamp (york-db-field lock "DateAdded"))
										(or (york-db-field lock "ThreadId") "")
										(or (york-db-field lock "Version") "")
										(or (york-db-field lock "Key1") "")
										(york--minutes-since lock "MinutesSince")))))

(defun york--insert-stalled-process (process)
	"Insert PROCESS, a row of `york--stalled-query', as an Org entry."
	(pcase-let* ((server (or (york-db-field process "server") ""))
							 (description (or (york-db-field process "description") ""))
							 (updated (york-db-field process "updated"))
							 (stamp (york--org-stamp updated))
							 (`(,date . ,time) (york--us-date-and-time updated))
							 (day (york--day-description)))
		(insert (format "*** %s: %s [%s]\n" server description stamp)
						":PROPERTIES:\n"
						(format ":Server: %s\n" server)
						(format ":Description: %s\n" description)
						(format ":Updated: [%s]\n" stamp)
						(format ":datedescription: %s\n" day)
						(format ":shortdate: %s\n" date)
						(format ":shorttime: %s\n" time)
						(format ":markdate: %s - %s %s\n" date day time)
						(format ":TotalMinutesSince: %.1f\n"
										(york--minutes-since process "minutes"))
						":END:\n\n")))

(defun york--insert-related-incidents (table server-column date-column server
																						 minutes &optional tag)
	"Insert the rows of TABLE about SERVER from the last MINUTES minutes.
SERVER-COLUMN and DATE-COLUMN are the columns to look in.  The rows
become Org entries, tagged with TAG if given; the session stats of the
sessions they mention follow."
	(let ((keys nil))
		(dolist (incident (york-db-query
											 (format "SELECT * FROM %s WHERE %s = %s
AND %s > (NOW() - INTERVAL %.4f MINUTE)"
															 table server-column (york-db-quote server)
															 date-column minutes)
											 'system t))
			(insert (format "**** %s [%s]%s\n"
											(or (york-db-field incident "Server") "")
											(york--org-stamp (york-db-field incident "DateAdded"))
											(if tag (concat " " tag) ""))
							":PROPERTIES:\n")
			(pcase-dolist (`(,name . ,value) incident)
				(when-let* ((key (york--session-key value)))
					(unless (member key keys)
						(push key keys)))
				(unless (string-match-p "info\\|updatemsg" (downcase name))
					(insert (format ":%s: %s\n" name (york--property-value value)))))
			(insert ":END:\n")
			(pcase-dolist (`(,name . ,value) incident)
				(when (string-match-p "info\\|updatemsg" (downcase name))
					(york--insert-block name "text" value))))
		(dolist (key (nreverse keys))
			(insert (format "*** %s Session Stats\n" key)
							"#+begin_src log-view\n"
							(org-escape-code-in-string (york--session-stats key))
							"\n#+end_src\n"))))

(defun york--snapshot-org (minutes)
	"Return a snapshot of the system as Org text.
A process counts as hung when it hasn't reported for MINUTES minutes,
and a lock as interesting when it is that old."
	(let* ((stalled (york-db-query (format york--stalled-query minutes) 'system))
				 (servers (seq-uniq (mapcar (lambda (process)
																			(or (york-db-field process "server") ""))
																		stalled)))
				 (locks (york-db-query
								 (concat
									(format "SELECT *, TIMESTAMPDIFF(SECOND, DateAdded, NOW()) / 60.0 AS MinutesSince
FROM lockstatus WHERE DateAdded < (NOW() - INTERVAL %d MINUTE)"
													minutes)
									(when servers
										(format " OR Server IN (%s)"
														(mapconcat #'york-db-quote servers ","))))
								 'foundation))
				 (other-locks (seq-remove (lambda (lock)
																		(member (york-db-field lock "Server") servers))
																	locks))
				 ;; How far back to look for incidents: to the oldest hang, and
				 ;; a little further.
				 (window (+ 5 (apply #'max 0.0
														 (mapcar (lambda (process)
																			 (york--minutes-since process "minutes"))
																		 stalled))))
				 (hung (format "%d %s hung" (length stalled)
											 (if (= (length stalled) 1) "process" "processes")))
				 (summary (string-join
									 (delq nil
												 (list (and stalled hung)
															 (and locks
																		(format "%d potentially interesting %s"
																						(length locks)
																						(if (cdr locks) "locks" "lock")))))
									 "; ")))
		(with-temp-buffer
			(insert (format "* System Snapshot (%s) %s\n"
											(if (string-empty-p summary) hung summary)
											(york--now-stamp))
							":PROPERTIES:\n"
							(format ":Query: %d minutes\n" minutes)
							":ClientAccount: York\n"
							(format ":ts: %s\n" (york--now-stamp)))
			(when stalled
				(insert ":COLUMNS:  %server(Server) %description(Process Description)"
								" %markdate(Last Updated) %totalminutessince(Minutes Since Last Update)\n"))
			(insert (format ":EXPORT_FILE_NAME: %sSystem Snapshot %s\n"
											york-workarea (york--file-stamp))
							":END:\n")
			(if (not stalled)
					(insert "\nNo hung processes to report.\n")
				(york--insert-break)
				(insert "#+begin: columnview :maxlevel 4 :skip-empty-rows t\n"
								"#+caption: Tabular style\n"
								"#+end:\n\n")
				(york--insert-break)
				(insert "#+caption: Mark's style\n"
								"#+begin_src org\n"
								(format "*%s - %s:*\n"
												(format-time-string "%-m/%-d/%Y") (york--day-description)))
				(dolist (process stalled)
					(insert (format "- %s %s %s. See attached screenshot.\n"
													(or (york-db-field process "server") "")
													(or (york-db-field process "description") "")
													(format-time-string "%-I:%M %p"))))
				(insert "#+end_src\n")
				(york--insert-break))
			(when other-locks
				(york--insert-locks
				 other-locks
				 (format "%d potentially interesting Foundation %s"
								 (length other-locks) (if (cdr other-locks) "locks" "lock"))))
			(dolist (server servers)
				(insert (format "** Potential %s issues\n" server))
				(when-let* ((its-locks (seq-filter (lambda (lock)
																						 (equal (york-db-field lock "Server") server))
																					 locks)))
					(york--insert-locks
					 its-locks
					 (format "%d potentially interesting Foundation %s for %s"
									 (length its-locks) (if (cdr its-locks) "locks" "lock") server)))
				(dolist (process (seq-filter (lambda (process)
																			 (equal (or (york-db-field process "server") "")
																							server))
																		 stalled))
					(york--insert-stalled-process process)
					(york--insert-related-incidents
					 "incidents" "Server" "LastUpdatedDate" server window ":noexport:")
					(york--insert-related-incidents
					 "extincidents" "ServerUpdating" "DateUpdated" server window)))
			(buffer-string))))

(defun york-get-hang-data (&optional minutes)
	"Insert a snapshot of hung processes into buffer in Org format.
A process counts as hung when it hasn't reported for 10 minutes; a
numeric prefix argument MINUTES changes that."
	(interactive "P")
	(insert (york--snapshot-org (if minutes (prefix-numeric-value minutes) 10))))

(provide 'york-incidents)
;;; york-incidents.el ends here
