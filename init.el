;;; init.el --- My init file. -*- lexical-binding: t; coding: utf-8-unix; -*-

(when (load-file "~/.emacs.d/emacs-setup.el")
	(kc/set-up-emacs)
	(kc/set-up-swiper))

(when (load-file "~/.emacs.d/org-setup.el")
	(kc/set-up-org))

(when at-work 
	(kc/load-file "~/.emacs.d/york-mode.el" 'error))

(kc/load-file "~/.personal.el" 'error)
(kc/load-file "~/.emacs.d/keybindings.el" 'error)
(kc/load-file "~/.emacs.d/abbrevs.el" 'message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (load-theme 'leuven-dark)
(require-package 'gruvbox-theme)
(load-theme 'gruvbox-dark-medium t)

(kc/load-file "~/.emacs.d/customize.el" 'message)

(server-start)
(message "init.el has been eval'd")
