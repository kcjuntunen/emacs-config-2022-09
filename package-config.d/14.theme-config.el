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
					 (load-theme 'leuven t))))

(kc/set-theme)
(global-set-key (kbd "C-c t") 'kc/set-theme)

(provide 'theme-config)
