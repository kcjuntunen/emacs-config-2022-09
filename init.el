;;; init.el --- My init file. -*- lexical-binding: t; coding: utf-8-unix; -*-
(defvar not-win (eq system-type 'gnu/linux)
	"If NOT-WIN is non-nil, then we're not in MS-Windows.")

(defvar at-work (not not-win)
	"Smyrno is the machine at work.")

(defvar kc/vp-font (if not-win
											 "IBM Plex Serif Text"
										 "Calibri")
	"My variable pitch font varies based on my OS.")

(defvar org-directory-root (if at-work
															 "C:/Users/k.c.juntunen/OneDrive/org/"
														 "~/Dropbox/org/")
	"The root upon which to build my org directory")

(load-file "~/.emacs.d/roman-numerals.el")

(setq org-directory
			(if not-win
					(concat org-directory-root (roman-year))
				(concat org-directory-root (format-time-string "%Y"))))

(setq org-personal-root
			(if at-work
					"C:/Users/k.c.juntunen/Documents/org"
				"~/Dropbox/org/"))

(setq org-personal-directory
			(concat org-personal-root (roman-year)))

(defun kc/set-up-emacs ()
	(setq-default blink-cursor-delay .2
								custom-file "~/.emacs.d/customize.el"
								blink-cursor-interval .2
								blink-cursor-blinks 10000
								tab-width 2
								fill-column 78
								column-number-mode t
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

(defun kc/set-up-org ()
	(setq-default
	 org-agenda-file-regexp "\\`[^.].*\\.org\\'"
	 kc/org-all-agenda-files
	 (directory-files
		(expand-file-name org-directory) t org-agenda-file-regexp)
	 org-startup-folded t
	 org-startup-indented t
	 org-indent-mode t
	 org-agenda-span 'day
	 org-fontify-quote-and-verse-blocks t
	 org-use-fast-todo-selection t
	 org-hide-emphasis-markers nil
	 org-treat-S-cursor-todo-selection-as-state-change nil
	 org-ellipsis " ▼ "
	 org-clock-continuously t
	 org-clock-out-remove-zero-time-clocks t
	 org-log-done 'time
	 org-refile-targets
	 (quote ((nil :maxlevel . 1) (kc/org-all-agenda-files :maxlevel . 2)))
	 org-catch-invisible-edits 'smart
	 org-agenda-clockreport-parameter-plist
	 '(:link t :maxlevel 4 :fileskip0 t
					 :properties
					 ("ClientAccount" "TradingPartnerAccount" "Request" "Phase" "Task"))
	 org-deadline-warning-days 45
	 org-agenda-window-setup 'current-window
	 org-agenda-skip-scheduled-if-done t
	 org-agenda-skip-deadline-if-done t
	 org-agenda-skip-timestamp-if-done t
	 org-agenda-log-mode-items '(closed clock state)
	 org-columns-default-format
	 "%25ITEM(Task) %40Description %20Captured %10Effort(Effort){:} %10CLOCKSUM"
	 org-global-properties
	 (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
					 ("STYLE_ALL" . "habit")))
	 org-todo-keywords
	 (quote ((sequence "TODO(t)" "WIP(n)" "|"
										 "DELEGATED(g)" "DONE(d)" "CANCELLED(c/!)")
					 (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|"
										 "CANCELLED(c/!)" "PHONE" "MEETING")))
	 org-todo-keyword-faces
	 (quote (("TODO" :foreground "red" :weight bold)
					 ("WIP" :foreground "blue" :weight bold)
					 ("DONE" :foreground "forest green" :weight bold)
					 ("WAITING" :foreground "orange" :weight bold)
					 ("HOLD" :foreground "magenta" :weight bold)
					 ("CANCELLED" :foreground "forest green" :weight bold)
					 ("MEETING" :foreground "forest green" :weight bold)
					 ("PHONE" :foreground "forest green" :weight bold)))
	 org-todo-state-tags-triggers
	 (quote (("CANCELLED" ("ARCHIVE" . t))
					 ("WAITING" ("WAITING" . t))
					 ("HOLD" ("WAITING") ("HOLD" . t))
					 (done ("WAITING") ("HOLD"))
					 ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
					 ("WIP" ("WAITING") ("CANCELLED") ("HOLD"))
					 ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
	 kc/interruption-file (concat org-directory "/unscheduled.org")
	 kc/refile-file (concat org-directory "/refile.org")
	 kc/diary-file (concat org-personal-directory "/diary.org")
	 kc/personal-notes-file (concat org-personal-directory "/notes.org")
	 kc/notes-file (concat org-directory "/notes.org")
	 kc/meetings-file (concat org-directory "/meetings.org")
	 org-capture-templates
	 '(("t" "todo" entry
			(file kc/refile-file)
			"* TODO %?
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:ClientAccount: %^{ClientAccount}
:TradingPartnerAccount: %^{TradingPartnerAccount}
:END:\n  SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n"
			:clock-in t :clock-resume t :prepend t)
		 ("p" "Phone call" entry
			(file kc/refile-file)
			"* PHONE %?
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:Prev_Loc: %K
:END:" :clock-in t :clock-resume t)
		 ("i" "Interuption" entry
			(file kc/interruption-file)
			"* %? %T
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:ClientAccount: %^{ClientAccount}
:TradingPartnerAccount: %^{TradingPartnerAccount}
:Prev_Loc: %K
:END:
:CLIPBOARD:\n%x\n:END:\n" :clock-in t :clock-resume t)
		 ("s" "Source Note" entry
			(file kc/refile-file)
			"* %?
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:Prev_Loc: %K
:END:
#+begin_source %^{Language|conf|csharp|powershell|sgml|shell|sql}\n%x\n#+end_source\n" :clock-in t :clock-resume t)
		 ("j" "Journal" entry
			(file+olp+datetree kc/diary-file)
			"* %? %T
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:Category: %^{Entry type|Bible|Note|Journal}
:END:
" :clock-in t :clock-resume t)
		 ("n" "Note" entry
			(file kc/personal-notes-file)
			"* %? :NOTE:
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:Prev_Loc: %K
:END:" :clock-in t :clock-resume t)
		 ("m" "Meeting" entry
			(file kc/meetings-file)
			"* MEETING %? %T
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Request: Not billable
:ClientAccount: %^{ClientAccount}
:TradingPartnerAccount: n/a
:Captured: %U
:Prev_Loc: %K
:END:" :clock-in t :clock-resume t)
		 ("b" "Bookmark" entry
			(file+headline kc/personal-notes-file "Bookmarks")
			"* %?\n:PROPERTIES:\n:Captured: %U\n:END:\n\n" :empty-lines 1))
	 org-clock-in-switch-to-state
	 (defun kc/clock-in-to-wip (kw)
		 "Switch from TODO to WIP when clocking in."
		 (when (not (and (boundp 'org-capture-mode) org-capture-mode))
			 (cond
				((member (org-get-todo-state) (list "TODO"))
				 "WIP")
				(t
				 kw)))))
	(setq org-agenda-files kc/org-all-agenda-files)
	(require 'time-stamp)
	(add-hook 'org-mode-hook 'flyspell-mode)
	(message "kc/set-up-org has been executed"))

;; This is here so PLINK can find my private key.
(eval-after-load "tramp"
	'(setf (cadr (assq 'tramp-login-args (cdr (assoc "plink" tramp-methods))))
				 '(("-l" "%u") ("-P" "%p") ("-i ~/.ssh/id_rsa.ppk")
					 ("-ssh") ("-t") ("%h") ("\"")
					 ("env 'TERM=dumb' 'PROMPT_COMMAND=' 'PS1=#$ '") ("/bin/sh") ("\""))))
(require 'package)
(add-to-list 'package-archives
						 '("melpa" . "https://stable.melpa.org/packages/") t)

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

(defun kc/org-check-agenda ()
	"Peek at agenda."
	(interactive)
	(cond
	 ((derived-mode-p 'org-agenda-mode)
		(if (window-parent) (delete-window) (bury-buffer)))
	 ((get-buffer "*Org Agenda*")
		(switch-to-buffer "*Org Agenda*"))
	 (t (org-agenda nil "a"))))

(define-key global-map (kbd "C-<f5>") 'org-agenda)
(define-key global-map (kbd "<f5>") 'kc/org-check-agenda)
(define-key global-map (kbd "<f6>") 'org-capture)
(define-key dired-mode-map (kbd "C-c C-w") 'wdired-change-to-wdired-mode)

(defun kc/load-file (filename func)
	"Try to load FILENAME. Execute FUNC on error."
	(interactive)
	(let ((expanded-filename (expand-file-name filename)))
		(if (not (file-exists-p expanded-filename))
				(eval `(,func (format "`%s' does not exist." ,filename)))
			(message "(KC) Loading %s..." filename)
			(load-file expanded-filename))))

(kc/set-up-emacs)
(kc/set-up-org)
(kc/set-up-swiper)
;; (load-theme 'leuven-dark)
(require-package 'gruvbox-theme)
(load-theme 'gruvbox-dark-medium t)

(kc/load-file "~/.personal.el" 'error)
(kc/load-file "~/.emacs.d/abbrevs.el" 'message)
(if at-work 
		(kc/load-file "~/.emacs.d/york-mode.el" 'error))
(server-start)

(kc/load-file (file-truename "~/.emacs.d/customize.el") 'message)
(message "init.el has been eval'd")
