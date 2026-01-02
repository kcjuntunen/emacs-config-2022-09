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
						(define-key map (kbd "C-c y n") 'york-get-incident-data)
						(define-key map (kbd "C-c y q") 'york-copy-queue-to-workarea)
						(define-key map (kbd "C-c y Q") 'york-copy-queue-to-workarea-and-open)
						(define-key map (kbd "C-c y s") 'york-store-repo-name)
						(define-key map (kbd "C-c y g") 'york-open-local-repo-name)
						(define-key map (kbd "C-c y G") 'york-open-remote-repo-name)
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

(defcustom york-request-looker-upper-path
	"C:/Users/k.c.juntunen/source/repos/Viewer.Etc/Experimental/bin/Debug/Experimental.exe"
	"The program that pulls in Projects/Phases/Tasks in Org format."
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
	(concat "//192.168.250.225/productionQueues/Outputq/" "outputq/"))

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
  "Start a Remote Desktop session using PROPERTY (default: SERVER) from the nearest Org property drawer."
  (interactive)
  (let* ((prop (or property "SERVER"))
				 (value (org-entry-get (point) prop t)))
    (unless value
      (setq value (kc/rdp-prompt-for-server))
			(org-set-property prop value))
    (kc/rdp-open value)))

;; (defun org-insert-window-screenshot ()
;;   "Launch the window screenshotter and insert the captured image inline in the current Org buffer.
;; Works reliably even if you switch buffers while waiting for the click."
;;   (interactive)
;;   (let* ((save-dir (expand-file-name org-screenshot-save-directory))
;;          (exe-path (expand-file-name org-screenshot-executable))
;;          (origin-buffer (current-buffer))
;;          (origin-point (point))
;;          (output-buffer (generate-new-buffer " *screenshot-output*"))
;;          process)

;;     (unless (file-exists-p exe-path)
;;       (error "Screenshot executable not found: %s" exe-path))

;;     (unless (file-directory-p save-dir)
;;       (make-directory save-dir t))

;;     (message "Screenshot tool launched. Click a window to capture... (ESC to cancel)")

;;     (setq process
;;           (start-process "window-screenshotter" output-buffer exe-path save-dir))

;;     (set-process-sentinel
;;      process
;;      (lambda (proc event)                     ; ← use "proc" here
;;        (when (and (eq (process-status proc) 'exit)
;;                   (string-match-p "finished" event))
;;          (with-current-buffer (process-buffer proc)  ; ← now proc is correctly bound
;;            (goto-char (point-min))
;;            (if (re-search-forward "^\\(.+\\.png\\)\\($\\|\\s-\\)" nil t)
;;                (let ((full-path (match-string 1)))
;;                  (with-current-buffer origin-buffer
;;                    (save-excursion
;;                      (goto-char origin-point)
;;                      ;; Ensure nice spacing around the image
;;                      (unless (looking-at-p "[[:space:]]*$")
;;                        (insert "\n"))
;;                      (insert "\n")
;;                      (insert (format "[[file:%s]]\n"
;;                                      (file-relative-name
;;                                       full-path
;;                                       (file-name-directory
;;                                        (or buffer-file-name default-directory)))))
;;                      ;; Refresh inline images in the region
;;                      (org-redisplay-inline-images)))
;;                  (message "Inserted screenshot: %s" (file-name-nondirectory full-path)))
;;              (message "No screenshot captured (possibly canceled with ESC).")))
;;          ;; Always clean up the output buffer
;;          (kill-buffer output-buffer))))))

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

(global-set-key (kbd "C-c y p") #'kc/rdp-open-at-point)

;;;###autoload
(add-hook 'org-mode-hook 'york-mode)

(message "york-mode loaded")
(provide 'york-mode)
