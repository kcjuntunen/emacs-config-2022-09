;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default custom-safe-themes
							'("93e5511b7f53825ef218a720093d5b320e9de8610e8e7a2aadf0b96a67d15dc9" ;; leuven-dark
								"92831daa963b36a6436ba2759f54161846e7af0958251cb5d033ddccc04d6bd3" ;; leuven
								default))

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
(set-face-attribute 'cursor nil :background "OrangeRed1")
(set-face-attribute 'fixed-pitch-serif nil :font "Victor Mono")
(set-face-attribute 'fixed-pitch nil :font "Victor Mono")
(set-face-attribute 'variable-pitch nil :font "Cambria")
(set-face-attribute 'default nil :font "Victor Mono")
(set-face-attribute 'org-table nil :height 0.9)
(set-face-attribute 'org-drawer nil :height 0.7)
(set-face-attribute 'org-meta-line nil :height 0.7)
(set-face-attribute 'org-block-begin-line nil :height 0.7)
(set-face-attribute 'org-block-end-line nil :height 0.7)))

(kc/set-theme)
(global-set-key (kbd "C-c t") 'kc/set-theme)

(provide 'theme-config)
