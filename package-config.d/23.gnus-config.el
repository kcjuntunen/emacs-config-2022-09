;;; 23.gnus-config.el --- Gmail in Gnus  -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal Gmail over IMAP and SMTP.  Gmail needs a 16-character app
;; password (2-Step Verification on), kept in authinfo as
;;   machine imap.gmail.com login ADDRESS port 993 password APP-PASSWORD
;;   machine smtp.gmail.com login ADDRESS port 587 password APP-PASSWORD
;; The From address is the IMAP login, looked up when composing.
;; Nothing connects until M-x gnus.

;;; Code:

(setq gnus-select-method '(nnimap "gmail"
																	(nnimap-address "imap.gmail.com")
																	(nnimap-server-port 993)
																	(nnimap-stream ssl))

			send-mail-function #'smtpmail-send-it
			smtpmail-smtp-server "smtp.gmail.com"
			smtpmail-smtp-service 587
			smtpmail-stream-type 'starttls

			;; Send from the Gmail address in authinfo (see 13.auth-config.el),
			;; without changing `user-mail-address' for the rest of Emacs.
			gnus-posting-styles '((".*"
														 (name "K. C. Juntunen")
														 (address (kc/auth-get-key "imap.gmail.com" :user))))

			;; Gmail files sent mail itself, so don't keep a second copy.
			gnus-message-archive-group nil

			;; Keep Gnus's files under var/ (untracked), and skip the .newsrc
			;; file that only newsreaders use.
			gnus-home-directory (locate-user-emacs-file "var/gnus/")
			gnus-directory (locate-user-emacs-file "var/gnus/News/")
			gnus-startup-file (locate-user-emacs-file "var/gnus/newsrc")
			message-directory (locate-user-emacs-file "var/gnus/Mail/")
			gnus-save-newsrc-file nil
			gnus-read-newsrc-file nil

			;; Show the plain-text version of a message when there is one.
			mm-discouraged-alternatives '("text/html" "text/richtext")

			;; Summary columns: marks, sent date, sender, thread tree, subject.
			gnus-summary-line-format "%U%R%z %&user-date;  %(%-20,20f%)  %B%s\n"
			gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M")))

(provide 'gnus-config)
;;; 23.gnus-config.el ends here
