;;; -*- lexical-binding: t; coding: utf-8-unix; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default custom-safe-themes
							'("93e5511b7f53825ef218a720093d5b320e9de8610e8e7a2aadf0b96a67d15dc9" ;; leuven-dark
								"92831daa963b36a6436ba2759f54161846e7af0958251cb5d033ddccc04d6bd3" ;; leuven
								default))

(defun kc/set-other-stuff ()
	"Set fonts according to descending preference."
	(let* ((fixed-width-fonts '("Victor Mono" "Monaco" "Consolas"))
				 (variable-width-fonts '("Publico Text" "Cambria"))
				 (the-font (car fixed-width-fonts)))
		(cl-loop for fnt in fixed-width-fonts do
						 (if (not (find-font (font-spec :name fnt)))
								 (message "%s: Nope" fnt)
							 (setq the-font fnt)))
		(message "Using %s" the-font)
		(set-face-attribute 'fixed-pitch-serif nil :font the-font)
		(set-face-attribute 'fixed-pitch nil :font the-font)
		(set-face-attribute 'default nil :font the-font)
		(cl-loop for fnt in variable-width-fonts do
						 (if (not (find-font (font-spec :name fnt)))
								 (message "%s: Nope" fnt)
							 (setq the-font fnt)))
		(message "Using %s" the-font)
		(set-face-attribute 'variable-pitch nil :font the-font)
		(set-face-attribute 'cursor nil :background "OrangeRed1")
		(set-face-attribute 'org-table nil :height 0.9)
		(set-face-attribute 'org-drawer nil :height 0.7)
		(set-face-attribute 'org-meta-line nil :height 0.7)
		(set-face-attribute 'org-block-begin-line nil :height 0.7)
		(set-face-attribute 'org-block-end-line nil :height 0.7)))

(defun kc/set-theme ()
	"Load theme depending on dark/light mode."
	(interactive)
	(let* ((dark-mode (when (fboundp 'w32-read-registry)
											(eq 0
													(w32-read-registry
													 'HKCU
													 "Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize"
													 "AppsUseLightTheme")))))
		(if dark-mode
				(load-theme 'leuven-dark t)
			(load-theme 'leuven t))
		(kc/set-other-stuff)))

(require 'org)
(kc/set-theme)
(global-set-key (kbd "C-c t") 'kc/set-theme)

(provide 'theme-config)
