;;; 20.outlook-calendar.el --- Outlook calendar in org-agenda  -*- lexical-binding: t; -*-

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

;; Work-only.  win-util/outlook-calendar.ps1 reads the running desktop
;; Outlook through COM and writes the calendar to outlook.org in
;; `org-directory', which makes it one of the agenda files.  The export
;; runs in the background at startup and every 30 minutes; run
;; `kc/outlook-calendar-refresh' to refresh by hand.
;;
;; The file is regenerated each time, so don't take notes in it.

;;; Code:

(require 'org)

(defvar at-work)   ; defined in early-init.el

(defvar kc/outlook-calendar-script
	(expand-file-name "win-util/outlook-calendar.ps1" user-emacs-directory)
	"PowerShell script that exports the Outlook calendar to Org.")

(defvar kc/outlook-calendar-days-back 7
	"How many days of past meetings to export.")

(defvar kc/outlook-calendar-days-ahead 21
	"How many days of upcoming meetings to export.")

(defvar kc/outlook-calendar-refresh-interval (* 30 60)
	"Seconds between background refreshes.")

(defvar kc/outlook-calendar--timer nil
	"Timer that refreshes the Outlook calendar.")

(defun kc/outlook-calendar-file ()
	"Return the Org file the Outlook calendar is written to."
	(expand-file-name "outlook.org" org-directory))

(defun kc/outlook-calendar--add-to-agenda ()
	"Make sure the calendar file is in the variable `org-agenda-files'."
	(let ((file (kc/outlook-calendar-file)))
		(when (and (listp org-agenda-files)
							 (file-exists-p file)
							 ;; file-equal-p, not member: Windows paths can differ in
							 ;; case ("C:/" vs "c:/") and still be the same file.
							 (not (seq-some (lambda (f) (file-equal-p f file)) org-agenda-files)))
			(setq org-agenda-files (append org-agenda-files (list file))))))

(defun kc/outlook-calendar-refresh ()
	"Export the Outlook calendar to Org in the background."
	(interactive)
	(if (not (file-directory-p org-directory))
			(message "Outlook calendar: %s doesn't exist" org-directory)
		(let ((buf (get-buffer-create " *outlook-calendar*"))
					(interactive-p (called-interactively-p 'interactive)))
			(with-current-buffer buf (erase-buffer))
			(make-process
			 :name "outlook-calendar"
			 :buffer buf
			 :noquery t
			 :command (list "powershell" "-NoProfile" "-NonInteractive"
											"-ExecutionPolicy" "Bypass"
											"-File" kc/outlook-calendar-script
											"-OutFile" (kc/outlook-calendar-file)
											"-DaysBack" (number-to-string kc/outlook-calendar-days-back)
											"-DaysAhead" (number-to-string kc/outlook-calendar-days-ahead))
			 :sentinel
			 (lambda (proc _event)
				 (when (memq (process-status proc) '(exit signal))
					 (let ((code (process-exit-status proc)))
						 (cond
							((zerop code)
							 (kc/outlook-calendar--add-to-agenda)
							 (when interactive-p
								 (message "Outlook calendar refreshed")))
							;; 2 = Outlook isn't running; only mention it when asked.
							((and (= code 2) (not interactive-p)))
							(t
							 (message "Outlook calendar: %s"
												(with-current-buffer (process-buffer proc)
													(string-trim (buffer-string)))))))))))))

(when at-work
	(when (timerp kc/outlook-calendar--timer)
		(cancel-timer kc/outlook-calendar--timer))
	(setq kc/outlook-calendar--timer
				(run-at-time 5 kc/outlook-calendar-refresh-interval
										 #'kc/outlook-calendar-refresh)))

(provide '20.outlook-calendar)
;;; 20.outlook-calendar.el ends here
