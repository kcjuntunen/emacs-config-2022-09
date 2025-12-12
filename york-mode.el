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
						(define-key map (kbd "C-c r") 'york-get-request-data)
						(define-key map (kbd "C-c n") 'york-get-incident-data)
						(define-key map (kbd "C-c q") 'york-copy-queue-to-workarea)
						(define-key map (kbd "C-c Q") 'york-copy-queue-to-workarea-and-open)
						(define-key map (kbd "C-c s") 'york-store-repo-name)
						(define-key map (kbd "C-c g") 'york-open-local-repo-name)
						(define-key map (kbd "C-c G") 'york-open-remote-repo-name)
						(define-key map (kbd "C-c i") 'york-org-insert-last-screenshot)
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

(defcustom york-request-looker-upper-path
	"C:/Users/k.c.juntunen/source/repos/Viewer.Etc/Experimental/bin/Debug/Experimental.exe"
	"The program that pulls in Projects/Phases/Tasks in Org format."
	:type 'string
	:group 'york)

(defcustom york-incident-looker-upper-path
	"C:/Users/k.c.juntunen/source/repos/Viewer.Etc/InsertIncident/bin/Release/net10.0/InsertIncident.exe"
	"The program that pulls in Projects/Phases/Tasks in Org format."
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

(defgroup york nil
	"Settings for `york-mode'."
	:prefix "york-"
	:group york-mode)

(defun york-store-repo-name (repo-name)
	"Store repo-name under the property name in `repo-name-property-name'."
	(interactive "sRepo Name: ")
	(org-entry-put (point) york--repo-name-property-name repo-name))

(defun york-get-request-data (request-number)
	"Insert Project/Phase/Task data into buffer in Org format."
	(interactive "sRequestNbr: ")
	(insert (shell-command-to-string
					 (format "%s %s"
									 york-request-looker-upper-path request-number))))

(defun york-get-incident-data (incident-number)
	"Insert incident data into buffer in Org format."
	(interactive "sIncident Number: ")
	(insert (shell-command-to-string
					 (format "%s %s"
									 york-incident-looker-upper-path incident-number))))

(defun york--get-inputq-path ()
	"Get the path to production input queues"
	(concat york-production-queue-path "incomeq/"))

(defun york--get-processq-path ()
	"Get the path to production process queues"
	(concat york-production-queue-path "processq/"))

(defun york--get-reprocessq-path ()
	"Get the path to production reprocess queues"
	(concat york-production-queue-path "reprocessq/"))

(defun york--get-outputq-path ()
	"Get the path to production output queues"
	(concat
	 york-production-queue-path "outputq/"))

(defun york--get-queue-path (queuekey)
	"Convert queue key to queue path."
	(let* ((queue-type (substring queuekey 0 1))
				 (qpath (cond ((string-equal queue-type "I") (york--get-inputq-path))
											((string-equal queue-type "P") (york--get-processq-path))
											((string-equal queue-type "R") (york--get-reprocessq-path))
											((string-equal queue-type "O") (york--get-outputq-path)))))
		(concat qpath queuekey ".dat")))

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
				(magit-status)))
		(cd here)))

(defun york-copy-queue-to-workarea (queuekey)
	"Copy payload indicated by QUEUEKEY to workarea."
	(interactive "sQueue Key: ")
	(let ((file-to-copy (york--get-queue-path queuekey))
				(destination (concat york-workarea queuekey ".dat")))
		(if (file-exists-p destination)
				(message "%s already exists" destination)
			(message "Copying %s to %s" file-to-copy york-workarea)
			(copy-file file-to-copy york-workarea))))

(defun york-copy-queue-to-workarea-and-open (queuekey)
	"Copy payload indicated by QUEUEKEY to workarea."
	(interactive "sQueue Key: ")
	(let ((file-to-copy (york--get-queue-path queuekey)))
		(york-copy-queue-to-workarea queuekey)
		(find-file (concat york-workarea queuekey ".dat"))))

(defun york-copy-queues-to-workarea (queue-keys)
	(cl-loop for queue-key in queue-keys
					 do (york-copy-queue-to-workarea queue-key)))

(defun york-org-insert-last-screenshot ()
	"After saving a screenshot insert it as an inline image."
	(interactive)
	(let ((most-recent-screenshot-path
				 (car (directory-files york-screenshot-searchpath t "^Screenshot.*png$" t))))
				(insert (format "[[file:%s]]" most-recent-screenshot-path))))

;; Bindings

;; TODO: Figure out why this works everywherem instead of only in org.
;; (map! :leader
;;       :after york-mode
;;       :map york-mode-map
;;       :mode york-mode
;;       (:prefix ("y" . "York")
;;        (:prefix ("r" . "Requests")
;;         :desc "Insert Request Data at point"
;;         :n "g" #'york-get-request-data)
;;        (:prefix ("s" . "Associated solution")
;;         :desc "Associate solution in default program from local repo"
;;         :n "s" #'york-store-repo-name)
;;        (:prefix ("s" . "Associated solution")
;;         :desc "Open associated solution in default program from local repo"
;;         :n "l" #'york-open-local-repo-name)
;;        (:prefix ("s" . "Associated solution")
;;         :desc "Open associated remote repo in Magit"
;;         :n "r" #'york-open-remote-repo-name)))

;;;###autoload
(add-hook 'org-mode-hook 'york-mode)

(message "york-mode loaded")
(provide 'york-mode)
