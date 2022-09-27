;;; york-mode.el --- a minor mode to contain functions for work -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Copyright (C) 2021 K. C. Juntunen

;; Author   : K. C. Juntunen <juntunen.kc@gmail.com>
;; URL      :
;; Package-Version:
;; Version  : 0.1
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
            (define-key map (kbd "C-c s") 'york-store-repo-name)
            (define-key map (kbd "C-c g") 'york-open-local-repo-name)
            (define-key map (kbd "C-c G") 'york-open-remote-repo-name)
            map)
  (if york-mode
      (message "york-mode activated")
    (message "york-mode deactivated")))


(defcustom york-request-looker-upper-path
  "D:/Source/Repos/Viewer.Etc/Experimental/bin/Debug/Experimental.exe"
  ;; "D:/Source/C#-Production/Viewer/Experimental/bin/Debug/Experimental.exe"
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

;;

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
