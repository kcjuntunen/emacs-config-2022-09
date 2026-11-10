;;; 21.york-db.el --- Query the FasTrack database  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  K. C. Juntunen

;; Author: K. C. Juntunen <k.c.juntunen@yorkwwt.com>
;; Keywords: data

;;; Commentary:

;; Runs queries against the generalhospital database with the mysql
;; command-line client.  The credentials come from the "yorkdb" entry
;; in authinfo.gpg.
;;
;; `york-db-query' runs a query and returns its rows;
;; `york-db-query-async' does the same in the background and hands the
;; rows to a callback.  A row is an alist of (COLUMN-NAME . VALUE),
;; where VALUE is a trimmed string, or nil for NULL; read it with
;; `york-db-field'.
;;
;; Queries are sent as they are, so don't build them from unchecked
;; input.

;;; Code:

(require 'seq)
(require 'subr-x)

(declare-function kc/auth-get-key "13.auth-config")
(declare-function kc/auth-get-pwd "13.auth-config")

(defvar york-db-host "proton"
	"Database server.")

(defvar york-db-database "generalhospital"
	"Database name.")

(defvar york-db-auth-host "yorkdb"
	"Host name of the authinfo entry holding the database credentials.")

(defun york-db--credentials ()
	"Return the database credentials as (USER . PASSWORD)."
	(let ((user (kc/auth-get-key york-db-auth-host :user)))
		;; Check before asking for the password: `kc/auth-get-pwd' fails
		;; badly when there is no entry.
		(unless user
			(user-error "No %s entry in authinfo" york-db-auth-host))
		(cons user (kc/auth-get-pwd york-db-auth-host))))

(defun york-db--command (user query)
	"Return the mysql command line that runs QUERY as USER."
	(list "mysql" "--batch" "--skip-ssl"
				"--default-character-set=utf8mb4"
				"--connect-timeout=15"
				"--host" york-db-host
				"--user" user
				"--database" york-db-database
				"--execute" query))

(defun york-db--environment (password)
	"Return a process environment that passes PASSWORD to mysql.
MYSQL_PWD keeps the password off the command line."
	(cons (concat "MYSQL_PWD=" password) process-environment))

(defun york-db--unescape (field)
	"Undo mysql --batch escaping in FIELD and trim it; NULL becomes nil.
Line breaks inside a value become spaces."
	(unless (string= field "NULL")
		(string-trim
		 (replace-regexp-in-string
			"\\\\\\(.\\)"
			(lambda (m)
				(pcase (aref m 1)
					(?t "\t") (?n " ") (?r "") (?0 "")
					(c (string c))))
			field t t))))

(defun york-db--parse (output)
	"Parse mysql --batch OUTPUT into a list of rows.
Each row is an alist of (COLUMN-NAME . VALUE)."
	(when-let* ((lines (split-string output "\n" t))
							(columns (split-string (car lines) "\t")))
		(mapcar (lambda (line)
							(seq-mapn #'cons columns
												(mapcar #'york-db--unescape (split-string line "\t"))))
						(cdr lines))))

(defun york-db-field (row column)
	"Return the value of COLUMN, a string, in ROW."
	(cdr (assoc column row)))

(defun york-db-query (query)
	"Run QUERY and return its rows.
Signal an error if mysql fails."
	(pcase-let ((`(,user . ,password) (york-db--credentials)))
		(with-temp-buffer
			(let* ((coding-system-for-read 'utf-8)
						 (coding-system-for-write 'utf-8)
						 (process-environment (york-db--environment password))
						 (command (york-db--command user query))
						 (status (apply #'call-process (car command) nil t nil (cdr command))))
				(unless (eq status 0)
					(error "mysql: %s" (string-trim (buffer-string))))
				(york-db--parse (buffer-string))))))

(defun york-db-query-async (query callback &optional on-error)
	"Run QUERY in the background and call CALLBACK with its rows.
If mysql fails, call ON-ERROR with its output instead; without
ON-ERROR, show the output as a message.  Missing credentials signal a
`user-error' right away."
	(pcase-let ((`(,user . ,password) (york-db--credentials)))
		(let ((process-environment (york-db--environment password)))
			(make-process
			 :name "york-db"
			 :buffer (generate-new-buffer " *york-db*")
			 :noquery t
			 :coding 'utf-8
			 :connection-type 'pipe
			 :command (york-db--command user query)
			 :sentinel
			 (lambda (proc _event)
				 (when (memq (process-status proc) '(exit signal))
					 (let ((output (with-current-buffer (process-buffer proc)
													 (buffer-string))))
						 (kill-buffer (process-buffer proc))
						 (cond
							((zerop (process-exit-status proc))
							 (funcall callback (york-db--parse output)))
							(on-error (funcall on-error (string-trim output)))
							(t (message "mysql: %s" (string-trim output)))))))))))

(provide '21.york-db)
;;; 21.york-db.el ends here
