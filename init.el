;;; init.el --- My init file. -*- lexical-binding: t; coding: utf-8-unix; -*-
(add-hook 'after-init-hook '(lambda () (load custom-file)))

(when (load-file "~/.emacs.d/emacs-setup.el")
	(kc/set-up-emacs)
	(kc/set-up-swiper))

(when (load-file "~/.emacs.d/org-setup.el")
	(kc/set-up-org))

(when at-work 
	(load "~/.emacs.d/york-mode.el" nil nil))

(load "~/.personal.el" nil nil)
(load "~/.emacs.d/keybindings.el" nil nil)
(load "~/.emacs.d/abbrevs.el" t nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (load-theme 'leuven-dark)
(require-package 'gruvbox-theme)
(load-theme 'gruvbox-dark-medium t)

(server-start)
(message "init.el has been eval'd")
