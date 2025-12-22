(defun kc/hang-wrapper ()
	"Call `york-get-hang-data' interactively."
	(with-temp-buffer
		(let ((standard-output (current-buffer)))
			(york-get-hang-data))
		(substring (buffer-string) 2)))

(add-to-list 'org-capture-templates
						 '("h" "Hangs"
							 entry
							 (file kc/monitor-file)
							 "* %?%(kc/hang-wrapper)"
							 :clock-in t
							 :clock-resume t))
