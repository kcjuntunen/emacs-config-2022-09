;;; org-setup.el --- Org set-up config.              -*- lexical-binding: t; -*-

;; Copyright (C) 2022  K. C. Juntunen

;; Author: K. C. Juntunen <k.c.juntunen@yorkwwt.com>
;; Keywords: lisp, files

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
(defvar org-directory-root (if at-work
															 "C:/Users/k.c.juntunen/OneDrive/org/"
														 "~/Dropbox/org/")
	"The root upon which to build my org directory")

(load-file "~/.emacs.d/roman-numerals.el")

(setq org-directory
			(if not-win
					(concat org-directory-root (roman-year))
				(concat org-directory-root (format-time-string "%Y"))))

(defvar org-personal-root
	(if at-work
			"C:/Users/k.c.juntunen/Documents/org/"
		"~/Dropbox/org/")
	"Where to store non-work-related Org files.")

(defvar org-personal-directory
	(concat org-personal-root (roman-year))
	"Org personal files under Roman numeral year.")

(defun kc/set-up-org ()
	"Enable Org config settings."
	(if (not (file-exists-p org-directory))
			(message "Skipping org setup. '%s' does not exixst" org-directory)
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
	 org-capture-templates '()
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
	(mapc #'load-file (directory-files "~/.emacs.d/org-capture.d/" t ".*el$"))
	(add-hook 'org-mode-hook 'flyspell-mode)
	(message "kc/set-up-org has been executed")))

(provide 'org-setup)
;;; org-setup.el ends here
