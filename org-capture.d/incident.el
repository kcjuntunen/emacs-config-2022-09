if (not at-work)
		(message "Not adding incident capture template.")
	(defun kc/incident-wrapper ()
		"Call `york-get-incident-data' interactively."
		(with-temp-buffer
			(let ((standard-output (current-buffer)))
				(call-interactively #'york-get-incident-data))
			(substring (buffer-string) 2)))

	(add-to-list 'org-capture-templates
							 '("e" "Incident"
								 entry
								 (file kc/interruption-file)
								 "* %?%(kc/incident-wrapper)"
								 :clock-in t
								 :clock-resume t)))
