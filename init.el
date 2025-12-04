;;; init.el --- My init file. -*- lexical-binding: t; coding: utf-8-unix; -*-
(defvar kc/quiet-message t 
	"So I can control how noisy (load) is.")

(defvar kc/quiet-errors t 
	"So I can control how noisy (load) is.")

(add-hook 'after-init-hook #'(lambda () (load custom-file kc/quiet-errors kc/quiet-message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load package config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mapc #'(lambda (f) (load f kc/quiet-errors kc/quiet-message))
			(directory-files "~/.emacs.d/package-config.d" t "^[0-9]\\{2\\}\\..*el$" nil))

(load "~/.emacs.d/keybindings.el" kc/quiet-errors kc/quiet-message)
(load "~/.emacs.d/abbrevs.el" kc/quiet-errors kc/quiet-message)

(when at-work 
	(load "~/.emacs.d/york-mode.el" kc/quiet-errors kc/quiet-message))

(load "~/.personal" kc/quiet-errors kc/quiet-message)

(require 'server)
(if (not (server-running-p))
		(server-start)
	(with-current-buffer "*scratch*"
		(insert (format "%s\n;; No emacs server"
						 (substitute-command-keys initial-scratch-message)))))

(if (not kc/quiet-message)
		(message "init.el has been eval'd"))
(message "Emacs started in %s seconds." (emacs-init-time))
