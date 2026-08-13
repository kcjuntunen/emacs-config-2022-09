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
							 "* %?%(kc/hang-wrapper)
:PROPERTIES:
:EXPORT_FILE_NAME: c:/fastrack/workarea/%(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:END:"
							 :clock-in t
							 :clock-resume t))
