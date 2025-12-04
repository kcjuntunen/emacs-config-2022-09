;;; emacs-config.el --- General GNU Emacs settings.   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  K. C. Juntunen

;; Author: K. C. Juntunen <k.c.juntunen@yorkwwt.com>
;; Keywords: lisp

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

;; 

;;; Code:
(defvar at-work (string-equal (downcase (system-name)) "smyrno")
	"Smyrno is the machine at work.")

(defun kc/set-up-emacs ()
	(setq-default blink-cursor-delay .2
								blink-cursor-interval .2
								blink-cursor-blinks 10000
								custom-file "~/.personal.el"
								tab-width 2
								fill-column 78
								column-number-mode t
								delete-by-moving-to-trash t
								vc-follow-symlinks t
								ring-bell-function 'ignore
								indent-tabs-mode t
								scroll-step 1
								sentence-end-double-space nil
								scroll-margin 0
								scroll-conservatively 100000
								scroll-preserve-screen-position 1
								show-paren-delay 0
								make-backup-files nil
								auto-save-default nil
								;; initial-frame-alist
								;; '((top . 1) (left . -1920) (width . 85) (height . 55))
								inhibit-startup-screen t)
	(blink-cursor-mode)
	(savehist-mode t)
	(electric-pair-mode t)
	(delete-selection-mode t)
	(global-auto-revert-mode t)
	(prefer-coding-system 'utf-8)
	(set-language-environment 'utf-8)
	(if (version< emacs-version "29")
			(add-hook 'prog-mode-hook 'linum-mode)
		(add-hook 'prog-mode-hook 'display-line-numbers-mode))
	(add-hook 'text-mode-hook 'flyspell-mode)
	(add-hook 'text-mode-hook 'auto-fill-mode)
	(global-set-key (kbd "C-c d") 'ediff-buffers)
	(if (not kc/quiet-message)
			(message "kc/set-up-emacs has been executed")))

;; This is here so PLINK can find my private key.
(when (not at-work)
	(eval-after-load "tramp"
		'(setf (cadr (assq 'tramp-login-args (cdr (assoc "plink" tramp-methods))))
					 '(("-l" "%u") ("-P" "%p") ("-i ~/.ssh/id_rsa.ppk")
						 ("-ssh") ("-t") ("%h") ("\"")
						 ("env 'TERM=dumb' 'PROMPT_COMMAND=' 'PS1=#$ '") ("/bin/sh") ("\"")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple package config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(savehist-mode 1)

;; Stolen from <https://pages.sachachua.com/.emacs.d/>
(defun kc/org-check-agenda ()
	"Peek at agenda."
	(interactive)
	(cond
	 ((derived-mode-p 'org-agenda-mode)
		(if (window-parent) (delete-window) (bury-buffer)))
	 ((get-buffer "*Org Agenda*")
		(switch-to-buffer (get-buffer "*Org Agenda*")))
	 (t (org-agenda nil "a"))))

(kc/set-up-emacs)

(provide 'emacs-config)
;;; emacs-config.el ends here
