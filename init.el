;;; init.el --- My init file. -*- lexical-binding: t; coding: utf-8-unix; -*-
(defvar kc/quiet-message t
	"So I can control how noisy (load) is.")

(defvar kc/quiet-errors t
	"So I can control how noisy (load) is.")

(when (load "~/.emacs.d/emacs-setup.el" kc/quiet-errors kc/quiet-message)
	(kc/set-up-emacs))

(when (load "~/.emacs.d/org-setup.el" kc/quiet-errors kc/quiet-message)
	(kc/set-up-org))

(when at-work 
	(load "~/.emacs.d/york-mode.el" kc/quiet-errors kc/quiet-message))

(add-hook 'after-init-hook '(lambda () (load custom-file kc/quiet-errors kc/quiet-message)))

(load "~/.personal.el" kc/quiet-errors kc/quiet-message)
(load "~/.emacs.d/keybindings.el" kc/quiet-errors kc/quiet-message)
(load "~/.emacs.d/abbrevs.el" kc/quiet-errors kc/quiet-message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (load-theme 'leuven-dark)
(load-theme 'gruvbox-dark-medium t)
(server-start)
(if (not kc/quiet-message)
		(message "init.el has been eval'd"))
