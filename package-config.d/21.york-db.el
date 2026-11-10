;;; 21.york-db.el --- Query the York databases  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  K. C. Juntunen

;; Author: K. C. Juntunen <k.c.juntunen@yorkwwt.com>
;; Keywords: data

;;; Commentary:

;; Runs queries against the York databases with the mysql command-line
;; client.  `york-db-connections' names the databases; the credentials
;; of each come from its entry in authinfo.gpg.
;;
;; `york-db-query' runs a query and returns its rows;
;; `york-db-query-async' does the same in the background and hands the
;; rows to a callback.  A row is an alist of (COLUMN-NAME . VALUE),
;; where VALUE is a trimmed string, or nil for NULL; read it with
;; `york-db-field'.
;;
;; Queries are sent as they are, so don't build them from unchecked
;; input; `york-db-quote' makes a string safe to use as a value.

;;; Code:

(require 'seq)
(require 'subr-x)

(declare-function kc/auth-get-key "13.auth-config")
(declare-function kc/auth-get-pwd "13.auth-config")

(defvar york-db-connections
	'((generalhospital :host "proton"  :database "generalhospital" :auth "yorkdb")
		(system          :host "Miranda" :database "System"          :auth "yorkdb-system")
		(foundation      :host "Umbriel" :database "Foundation"      :auth "yorkdb-foundation"))
	"The databases `york-db-query' can reach, as (NAME . PLIST).
In PLIST, :host is the server, :database the database and :auth the
host name of the authinfo entry holding the user and password.")

(defun york-db--connection (name)
	"Return the plist of the connection called NAME (default generalhospital)."
	(or (alist-get (or name 'generalhospital) york-db-connections)
			(error "Unknown database connection: %s" name)))

(defun york-db--credentials (auth-host)
	"Return the credentials of the authinfo entry AUTH-HOST as (USER . PASSWORD)."
	(let ((user (kc/auth-get-key auth-host :user)))
		;; Check before asking for the password: `kc/auth-get-pwd' fails
		;; badly when there is no entry.
		(unless user
			(user-error "No %s entry in authinfo" auth-host))
		(cons user (kc/auth-get-pwd auth-host))))

(defun york-db--command (connection user query)
	"Return the mysql command line that runs QUERY as USER on CONNECTION."
	(list "mysql" "--batch" "--skip-ssl"
				"--default-character-set=utf8mb4"
				"--connect-timeout=15"
				"--host" (plist-get connection :host)
				"--user" user
				"--database" (plist-get connection :database)
				"--execute" query))

(defun york-db--environment (password)
	"Return a process environment that passes PASSWORD to mysql.
MYSQL_PWD keeps the password off the command line."
	(cons (concat "MYSQL_PWD=" password) process-environment))

(defun york-db--unescape (field &optional keep-newlines)
	"Undo mysql --batch escaping in FIELD and trim it; NULL becomes nil.
Line breaks inside a value become spaces, unless KEEP-NEWLINES."
	(unless (string= field "NULL")
		(string-trim
		 (replace-regexp-in-string
			"\\\\\\(.\\)"
			(lambda (m)
				(pcase (aref m 1)
					(?t "\t") (?n (if keep-newlines "\n" " ")) (?r "") (?0 "")
					(c (string c))))
			field t t))))

(defun york-db--parse (output &optional keep-newlines)
	"Parse mysql --batch OUTPUT into a list of rows.
Each row is an alist of (COLUMN-NAME . VALUE).  KEEP-NEWLINES is as
in `york-db--unescape'."
	(when-let* ((lines (split-string output "\n" t))
							(columns (split-string (car lines) "\t")))
		(mapcar (lambda (line)
							(seq-mapn #'cons columns
												(mapcar (lambda (field)
																	(york-db--unescape field keep-newlines))
																(split-string line "\t"))))
						(cdr lines))))

(defun york-db-field (row column)
	"Return the value of COLUMN, a string, in ROW.
Like MySQL, this ignores the case of COLUMN."
	(cdr (assoc-string column row t)))

(defun york-db-quote (string)
	"Return STRING as an SQL string literal, quotes included."
	(concat "'"
					(replace-regexp-in-string "['\\]" (lambda (c) (concat "\\" c))
																		string t t)
					"'"))

(defun york-db-query (query &optional connection keep-newlines)
	"Run QUERY and return its rows.
CONNECTION names an entry of `york-db-connections'; the default is
generalhospital.  Line breaks inside a value become spaces, unless
KEEP-NEWLINES.  Signal an error if mysql fails."
	(pcase-let* ((connection (york-db--connection connection))
							 (`(,user . ,password)
								(york-db--credentials (plist-get connection :auth))))
		(with-temp-buffer
			(let* ((coding-system-for-read 'utf-8)
						 (coding-system-for-write 'utf-8)
						 (process-environment (york-db--environment password))
						 (command (york-db--command connection user query))
						 (status (apply #'call-process (car command) nil t nil (cdr command))))
				(unless (eq status 0)
					(error "mysql: %s" (string-trim (buffer-string))))
				(york-db--parse (buffer-string) keep-newlines)))))

(defun york-db-query-async (query callback &optional on-error connection)
	"Run QUERY in the background and call CALLBACK with its rows.
If mysql fails, call ON-ERROR with its output instead; without
ON-ERROR, show the output as a message.  CONNECTION is as in
`york-db-query'.  Missing credentials signal a `user-error' right
away."
	(pcase-let* ((connection (york-db--connection connection))
							 (`(,user . ,password)
								(york-db--credentials (plist-get connection :auth))))
		(let ((process-environment (york-db--environment password)))
			(make-process
			 :name "york-db"
			 :buffer (generate-new-buffer " *york-db*")
			 :noquery t
			 :coding 'utf-8
			 :connection-type 'pipe
			 :command (york-db--command connection user query)
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
