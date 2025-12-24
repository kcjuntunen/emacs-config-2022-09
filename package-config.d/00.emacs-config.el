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
	(if at-work
			(add-hook 'text-mode-hook 'auto-fill-mode)
		(add-hook 'text-mode-hook 'visual-line-mode))
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
(if at-work
		(setq kc/exec-path
					'("C:/Program Files/ImageMagick-7.1.0-Q16"
						"C:/Program Files/nodejs/"
						"C:/Users/k.c.juntunen/emax64/bin"
						"C:/Users/k.c.juntunen/bin/PortableGit/bin"
						"C:/Users/k.c.juntunen/.dotnet/tools"
						"C:/Users/k.c.juntunen/AppData/Local/Programs/Fiddler"
						"C:/Program Files (x86)/Gpg4win/bin"
						"C:/Users/k.c.juntunen/opt/Hunspell/bin"
						"C:/Program Files/Java/jdk1.8.0_333/bin"
						"C:/Users/k.c.juntunen/opt/apache-maven-3.8.6/bin"
						"C:/Users/k.c.juntunen/AppData/Local/Pandoc/"
						"C:/Users/k.c.juntunen/opt/ripgrep-13.0.0-x86_64-pc-windows-gnu"
						"C:/Users/k.c.juntunen/opt/fd-v8.4.0-x86_64-pc-windows-gnu"
						"C:/Users/k.c.juntunen/AppData/Local/Microsoft/WindowsApps"
						"C:/Program Files/nodejs"
						"C:/Program Files/Azure Data Studio/bin"
						"C:/Program Files/MariaDB 11.2/bin"
						"C:/Users/k.c.juntunen/opt/fakeerp"
						"C:/Users/k.c.juntunen/bin/PortableGit/usr/bin"
						"C:/Users/k.c.juntunen/opt/php"
						"C:/Users/k.c.juntunen/AppData/Roaming/npm"
						"C:/Users/k.c.juntunen/AppData/Local/PowerToys/DSCModules/"))
	(let ((path (string-join kc/exec-path ";")))
		(setenv "PATH" path)
		(setq exec-path kc/exec-path)))

(kc/set-up-emacs)

(provide 'emacs-config)
;;; emacs-config.el ends here
