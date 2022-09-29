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
		(add-hook 'prog-mode-hook 'display-line-numbers-mode)
		(load-file "~/git/ligature.el/ligature.el")
		(require 'ligature)
		(ligature-set-ligatures 't '("www"))
		;; Enable traditional ligature support in eww-mode, if the
		;; `variable-pitch' face supports it
		(ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
		;; Enable all Cascadia Code ligatures in programming modes
		(ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
																				 ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
																				 "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
																				 "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
																				 "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
																				 "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
																				 "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
																				 "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
																				 ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
																				 "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
																				 "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
																				 "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
																				 "\\\\" "://"))
		(global-ligature-mode 1))
	(add-hook 'text-mode-hook 'flyspell-mode)
	(message "kc/set-up-emacs has been executed"))

;; This is here so PLINK can find my private key.
(eval-after-load "tramp"
	'(setf (cadr (assq 'tramp-login-args (cdr (assoc "plink" tramp-methods))))
				 '(("-l" "%u") ("-P" "%p") ("-i ~/.ssh/id_rsa.ppk")
					 ("-ssh") ("-t") ("%h") ("\"")
					 ("env 'TERM=dumb' 'PROMPT_COMMAND=' 'PS1=#$ '") ("/bin/sh") ("\""))))
(require 'package)

(unless (assoc-default "melpa" package-archives)
	(add-to-list 'package-archives
							 '("melpa" . "https://stable.melpa.org/packages/") t))
(unless (assoc-default "nognu" package-archives)
	(add-to-list 'package-archives
							 '("nognu" . "https://elpa.nongnu.org/nongnu/") t))

;;; from purcell/emacs.d
(defun require-package (package &optional min-version no-refresh)
	"Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
	(if (package-installed-p package min-version)
			t
		(if (or (assoc package package-archive-contents) no-refresh)
				(package-install package)
			(progn
				(package-refresh-contents)
				(require-package package min-version t)))))

(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'evil)

(setq evil-search-module 'evil-search
			evil-want-C-u-scroll t
			evil-want-C-w-in-emacs-state t)

(require 'evil)
(evil-mode t)
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'which-key)
(require 'which-key)
(which-key-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'ivy)
(require 'ivy)
(ivy-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; counsel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'counsel)
(require 'counsel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; swiper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'swiper)
(require 'swiper)
(defun kc/set-up-swiper ()
	(setq ivy-use-virtual-buffers t)
	(setq enable-recursive-minibuffers t)
	;; enable this if you want `swiper' to use it
	;; (setq search-default-mode #'char-fold-to-regexp)
	(global-set-key "\C-s" 'swiper)
	(global-set-key (kbd "C-c C-r") 'ivy-resume)
	;; (global-set-key (kbd "<f6>") 'ivy-resume)
	(global-set-key (kbd "M-x") 'counsel-M-x)
	(global-set-key (kbd "C-x C-f") 'counsel-find-file)
	(global-set-key (kbd "<f1> f") 'counsel-describe-function)
	(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
	(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
	(global-set-key (kbd "<f1> l") 'counsel-find-library)
	(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
	(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
	(global-set-key (kbd "C-c g") 'counsel-git)
	(global-set-key (kbd "C-c j") 'counsel-git-grep)
	(global-set-key (kbd "C-c k") 'counsel-ag)
	(global-set-key (kbd "C-x l") 'counsel-locate)
	(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
	(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'company)
(require 'company)
(add-hook 'prog-mode-hook 'company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cobol-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when at-work
	(require-package 'cobol-mode)
	(require 'cobol-mode)
	(add-to-list 'auto-mode-alist '("\\.cbl\\'" . cobol-mode))
	(add-to-list 'auto-mode-alist '("\\.cpy\\'" . cobol-mode))
	(add-hook 'cobol-mode-hook 'ruler-mode))

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

(defun kc/load-file (filename func)
	"Try to load FILENAME. Execute FUNC on error."
	(interactive)
	(let ((expanded-filename (expand-file-name filename)))
		(if (not (file-exists-p expanded-filename))
				(eval `(,func (format "`%s' does not exist." ,filename)))
			(message "(KC) Loading %s..." filename)
			(load-file expanded-filename))))

(provide 'emacs-setup)
;;; emacs-setup.el ends here
