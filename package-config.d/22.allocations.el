;;; 22.allocations.el --- FasTrack allocations in org-agenda  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  K. C. Juntunen

;; Author: K. C. Juntunen <k.c.juntunen@yorkwwt.com>
;; Keywords: calendar, outlines

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Work-only.  Runs the resource-allocations query against the
;; generalhospital database with the mysql client and writes the open
;; (not completed) allocations to allocations.org in `org-directory',
;; which makes it one of the agenda files.  Each day gets a total entry
;; plus one entry per request, dated with an active timestamp so it
;; shows on that day in the agenda.
;;
;; The export runs in the background at startup and every 30 minutes;
;; run `kc/allocations-refresh' to refresh by hand.  The database
;; credentials come from the "yorkdb" entry in authinfo.gpg.
;;
;; The file is regenerated each time, so don't take notes in it.

;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)

(defvar at-work)   ; defined in early-init.el
(declare-function kc/auth-get-key "13.auth-config")
(declare-function kc/auth-get-pwd "13.auth-config")

(defvar kc/allocations-resource-key 15
	"Resource key (resource_master.Key1) whose allocations to export.")

(defvar kc/allocations-db-host "proton"
	"Database server.")

(defvar kc/allocations-database "generalhospital"
	"Database name.")

(defvar kc/allocations-auth-host "yorkdb"
	"Host name of the authinfo entry holding the database credentials.")

(defvar kc/allocations-days-back 5
	"How many days of past allocations to export.")

(defvar kc/allocations-days-ahead 14
	"How many days of upcoming allocations to export.")

(defvar kc/allocations-refresh-interval (* 30 60)
	"Seconds between background refreshes.")

(defvar kc/allocations--timer nil
	"Timer that refreshes the allocations.")

(defconst kc/allocations--query
	"SELECT
  DATE_FORMAT(ra.AllocationDateTime, '%%Y-%%m-%%d') AS allocated,
  SUM(ra.AllocatedMinutes) AS minutes,
  ra.RequestNbr AS request,
  GROUP_CONCAT(DISTINCT CONCAT_WS(':', ra.PhaseNbr, ra.TaskNbr)
    ORDER BY CAST(ra.TaskNbr AS INT) SEPARATOR ';') AS tasks,
  GROUP_CONCAT(DISTINCT t.Description
    ORDER BY CAST(t.TaskNbr AS INT) SEPARATOR '; ') AS descriptions,
  cr.Status AS status,
  cr.ClientAccount AS client,
  cr.TradingPartnerAccount AS partner,
  t.IsTaskCompletedInd AS iscomplete
FROM resource_allocations ra
LEFT JOIN clientrequests cr ON ra.RequestNbr = cr.RequestNbr
LEFT JOIN tasks t ON ra.RequestNbr = t.RequestNbr
  AND ra.PhaseNbr = t.PhaseNbr AND ra.TaskNbr = t.TaskNbr
WHERE ra.ResourceKey = %d
  AND ra.AllocationDateTime BETWEEN (CURDATE() - INTERVAL %d DAY)
                                AND (CURDATE() + INTERVAL %d DAY)
GROUP BY DATE(ra.AllocationDateTime), ra.RequestNbr
ORDER BY DATE(ra.AllocationDateTime), ra.RequestNbr"
	"Allocations query; format args are resource key, days back, days ahead.
Same as the one in the Allocations subtree, except that it uses
CURDATE so whole days are included at both ends.  The columns are
read by position in `kc/allocations--write'.")

(defun kc/allocations-file ()
	"Return the Org file the allocations are written to."
	(expand-file-name "allocations.org" org-directory))

(defun kc/allocations--add-to-agenda ()
	"Make sure the allocations file is in the variable `org-agenda-files'."
	(let ((file (kc/allocations-file)))
		(when (and (listp org-agenda-files)
							 (file-exists-p file)
							 ;; file-equal-p, not member: Windows paths can differ in
							 ;; case ("C:/" vs "c:/") and still be the same file.
							 (not (seq-some (lambda (f) (file-equal-p f file)) org-agenda-files)))
			(setq org-agenda-files (append org-agenda-files (list file))))))

(defun kc/allocations--unescape (field)
	"Undo mysql --batch escaping in FIELD and trim it; NULL becomes nil."
	(unless (string= field "NULL")
		(string-trim
		 (replace-regexp-in-string
			"\\\\\\(.\\)"
			(lambda (m)
				(pcase (aref m 1)
					(?t "\t") (?n " ") (?r "") (?0 "")
					(c (string c))))
			field t t))))

(defun kc/allocations--parse (output)
	"Parse mysql --batch OUTPUT into a list of rows, skipping the header."
	(mapcar (lambda (line)
						(mapcar #'kc/allocations--unescape (split-string line "\t")))
					(cdr (split-string output "\n" t))))

(defun kc/allocations--duration (minutes)
	"Format MINUTES as H:MM, the way Org writes efforts."
	(format "%d:%02d" (/ minutes 60) (% minutes 60)))

(defun kc/allocations--short-duration (minutes)
	"Format MINUTES as 45m or 1h15m for headings.
Not H:MM: the agenda would read that as a time of day."
	(let ((h (/ minutes 60)) (m (% minutes 60)))
		(cond ((zerop h) (format "%dm" m))
					((zerop m) (format "%dh" h))
					(t (format "%dh%02dm" h m)))))

(defun kc/allocations--stamp (date)
	"Return an active Org timestamp for DATE, a YYYY-MM-DD string."
	(let ((d (parse-time-string date)))
		(format-time-string
		 "<%Y-%m-%d %a>"
		 (encode-time (list 0 0 12 (nth 3 d) (nth 4 d) (nth 5 d))))))

(defun kc/allocations--write (rows file)
	"Write allocation ROWS to FILE as Org."
	(with-temp-file file
		(insert "#+title: Allocations\n"
						;; "allocations" fills the agenda's category column.
						"#+category: alloc\n"
						"#+startup: overview\n"
						"# Generated by kc/allocations-refresh; changes will be lost.\n"
						(format "# Updated %s\n" (format-time-string "%Y-%m-%d %H:%M")))
		(dolist (day (seq-group-by #'car rows))
			(let* ((date (car day))
						 (stamp (kc/allocations--stamp date))
						 (entries (cdr day))
						 (total (apply #'+ (mapcar (lambda (r) (string-to-number (nth 1 r)))
																			 entries))))
				(insert (format "\n* Allocated %s (%d request%s)\n%s\n"
												(kc/allocations--short-duration total)
												(length entries) (if (= (length entries) 1) "" "s")
												stamp))
				(pcase-dolist (`(,_ ,minutes ,request ,tasks ,descriptions ,status ,client ,partner ,iscomplete)
											 entries)
					(let ((minutes (string-to-number minutes)))
						(insert (format "** %s [%s] %s %s — %s / %s\n"
														(if (string-equal-ignore-case IsComplete "Y") "DONE" "TODO")
														(kc/allocations--short-duration minutes)
														request
														(or descriptions "(no description)")
														(or client "?") (or partner "?"))
										":PROPERTIES:\n"
										(format ":Effort: %s\n" (kc/allocations--duration minutes))
										(format ":REQUEST: %s\n" request)
										(format ":TASKS: %s\n" (or tasks ""))
										(format ":STATUS: %s\n" (or status ""))
										":END:\n"
										stamp "\n")))))))

(defun kc/allocations--revert-buffer (file)
	"Reload FILE's buffer, if it is visited and unmodified.
The agenda reads visited files from their buffers, so a stale buffer
would hide the new allocations."
	(let ((buf (find-buffer-visiting file)))
		(when (and buf (not (buffer-modified-p buf)))
			(with-current-buffer buf
				;; Not preserve-modes: Org rereads #+category etc. only when
				;; the mode starts.
				(revert-buffer t t)))))

(defun kc/allocations-refresh ()
	"Export open allocations to Org in the background."
	(interactive)
	(let ((interactive-p (called-interactively-p 'interactive))
				(user (kc/auth-get-key kc/allocations-auth-host :user))
				(pwd (kc/auth-get-pwd kc/allocations-auth-host)))
		(cond
		 ((not (file-directory-p org-directory))
			(message "Allocations: %s doesn't exist" org-directory))
		 ((not (and user pwd))
			(message "Allocations: no %s entry in authinfo" kc/allocations-auth-host))
		 (t
			(let ((buf (get-buffer-create " *allocations*"))
						;; MYSQL_PWD keeps the password off the command line.
						(process-environment (cons (concat "MYSQL_PWD=" pwd)
																			 process-environment)))
				(with-current-buffer buf (erase-buffer))
				(make-process
				 :name "allocations"
				 :buffer buf
				 :noquery t
				 :coding 'utf-8
				 :connection-type 'pipe
				 :command (list "mysql" "--batch" "--skip-ssl"
												"--default-character-set=utf8mb4"
												"--connect-timeout=15"
												"--host" kc/allocations-db-host
												"--user" user
												"--database" kc/allocations-database
												"--execute"
												(format kc/allocations--query
																kc/allocations-resource-key
																kc/allocations-days-back
																kc/allocations-days-ahead))
				 :sentinel
				 (lambda (proc _event)
					 (when (memq (process-status proc) '(exit signal))
						 (let ((output (with-current-buffer (process-buffer proc)
														 (buffer-string))))
							 (if (zerop (process-exit-status proc))
									 (let ((file (kc/allocations-file)))
										 (kc/allocations--write (kc/allocations--parse output) file)
										 (kc/allocations--revert-buffer file)
										 (kc/allocations--add-to-agenda)
										 (when interactive-p
											 (message "Allocations refreshed")))
								 (message "Allocations: %s" (string-trim output))))))))))))

(when at-work
	(when (timerp kc/allocations--timer)
		(cancel-timer kc/allocations--timer))
	(setq kc/allocations--timer
				(run-at-time 10 kc/allocations-refresh-interval
										 #'kc/allocations-refresh)))

(provide '22.allocations)
;;; 22.allocations.el ends here
