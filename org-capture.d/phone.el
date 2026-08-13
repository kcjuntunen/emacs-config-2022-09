(if (not at-work)
		(message "Not adding phone capture template.")
	(add-to-list 'org-capture-templates
							 '("p" "Phone call" entry
								 (file kc/refile-file)
								 "* PHONE %^{Heading}
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:Prev_Loc: %K
:EXPORT_FILE_NAME: c:/fastrack/workarea/%(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:END:
%?"
								 :clock-in t
								 :clock-resume t)))
