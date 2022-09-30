;;; emacs-setup.el --- General GNU Emacs settings.   -*- lexical-binding: t; -*-

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
(defvar not-win (eq system-type 'gnu/linux)
	"If NOT-WIN is non-nil, then we're not in MS-Windows.")

(defvar at-work (not not-win)
	"Smyrno is the machine at work.")

(defvar kc/vp-font (if not-win
											 "IBM Plex Serif Text"
										 "Calibri")
	"My variable pitch font varies based on my OS.")

(defun kc/set-up-emacs ()
	(setq-default blink-cursor-delay .2
								blink-cursor-interval .2
								blink-cursor-blinks 10000
								custom-file "~/.emacs.d/customize.el"
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
								tool-bar-mode -1
								menu-bar-mode t
								inhibit-startup-screen t)
	(blink-cursor-mode)
	(savehist-mode t)
	(electric-pair-mode t)
	(delete-selection-mode t)
	(global-auto-revert-mode t)
	(prefer-coding-system 'utf-8)
	(set-language-environment 'utf-8)
	(add-to-list 'default-frame-alist '(height . 40))
	(add-to-list 'default-frame-alist '(width . 80))

	(if (version< emacs-version "29")
			(add-hook 'prog-mode-hook 'linum-mode)
		(add-hook 'prog-mode-hook 'display-line-numbers-mode))

	(add-hook 'text-mode-hook 'flyspell-mode)
	(message "kc/set-up-emacs has been executed"))

;; This is here so PLINK can find my private key.
(eval-after-load "tramp"
	'(setf (cadr (assq 'tramp-login-args (cdr (assoc "plink" tramp-methods))))
				 '(("-l" "%u") ("-P" "%p") ("-i ~/.ssh/id_rsa.ppk")
					 ("-ssh") ("-t") ("%h") ("\"")
					 ("env 'TERM=dumb' 'PROMPT_COMMAND=' 'PS1=#$ '") ("/bin/sh") ("\""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load package config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mapc #'(lambda (f) (load f nil nil))
			(directory-files "~/.emacs.d/package-config.d" t ".*el$" nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; savehist-mode
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
		(switch-to-buffer "*Org Agenda*"))
	 (t (org-agenda nil "a"))))

(provide 'emacs-setup)
;;; emacs-setup.el ends here
