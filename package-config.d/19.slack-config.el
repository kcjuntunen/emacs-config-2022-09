;;; 19.slack-config.el --- Set up emacs-slack  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  K. C. Juntunen

;; Author: K. C. Juntunen <k.c.juntunen@yorkwwt.com>
;; Keywords: comm

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

;; Work-only Slack client.  emacs-slack isn't on MELPA stable, so it's
;; installed from GitHub with use-package's :vc keyword.
;;
;; Credentials come from authinfo (see 13.auth-config.el).  Get the
;; xoxc- token and the `d' cookie (xoxd-...) from a logged-in browser
;; session, then add:
;;
;;   machine yorkworldwide-vbl9255.slack.com login k.c.juntunen@yorkwwt.com password xoxc-...
;;   machine yorkworldwide-vbl9255.slack.com login k.c.juntunen@yorkwwt.com^cookie password xoxd-...
;;
;; Start with C-c S s, which connects every registered team.  Commands
;; act on the current team, York at first; C-c S T changes it.

;;; Code:

(defvar at-work)   ; defined in early-init.el

(declare-function slack-start "slack")

(defconst kc/slack-host "yorkworldwide-vbl9255.slack.com"
	"Slack workspace domain, also the authinfo machine name.")

(defconst kc/slack-user "k.c.juntunen@yorkwwt.com"
	"Login used for the Slack entries in authinfo.")

(defconst kc/rk-slack-host "rkdevelopers.slack.com"
	"Slack workspace domain, also the authinfo machine name.")

(defun kc/slack-start-all ()
	"Connect to every registered Slack team.
Called as a command, `slack-start' connects only the current team."
	(interactive)
	(slack-start))

(use-package slack
	:if at-work
	:vc (:url "https://github.com/emacs-slack/emacs-slack" :rev :newest)
	:commands (slack-start)
	:bind (("C-c S s" . kc/slack-start-all)
				 ("C-c S T" . slack-change-current-team)
				 ("C-c S c" . slack-channel-select)
				 ("C-c S i" . slack-im-select)
				 ("C-c S u" . slack-select-unread-rooms)
				 ("C-c S t" . slack-all-threads))
	:custom
	(slack-buffer-emojify nil)
	;; Default avatars come from Gravatar's CDN (i0/i1/i2.wp.com), which
	;; is unreachable from the work network.
	(slack-render-profile-images-p nil)
	(slack-prefer-current-team t)
	;; Show "[ York: <mentions>, <thread mentions> ]" in the mode line,
	;; counting every conversation (no channels are subscribed).
	(slack-enable-global-mode-string t)
	(slack-modeline-count-only-subscribed-channel nil)
	:config
	(slack-register-team
	 :name "York"
	 :modeline-name "ywt"
	 :default t
	 :token (auth-source-pick-first-password
					 :host kc/slack-host :user kc/slack-user)
	 :cookie (auth-source-pick-first-password
						:host kc/slack-host :user (concat kc/slack-user "^cookie"))
	 :full-and-display-names t
	 :modeline-enabled t)
	;; Only York is :default: the last team registered with it becomes the
	;; current one.
	(slack-register-team
	 :name "Rural King"
	 :modeline-name "rk"
	 :token (auth-source-pick-first-password
					 :host kc/rk-slack-host :user kc/slack-user)
	 :cookie (auth-source-pick-first-password
						:host kc/rk-slack-host :user (concat kc/slack-user "^cookie"))
	 :full-and-display-names t
	 :modeline-enabled t))

(provide '19.slack-config)
;;; 19.slack-config.el ends here
