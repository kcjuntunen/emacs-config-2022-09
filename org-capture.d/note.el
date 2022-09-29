(add-to-list 'org-capture-templates
						 '("n" "Note" entry
							 (file kc/personal-notes-file)
							 "* %^{Heading} :NOTE:
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:END:
%?"
							 :clock-in t
							 :clock-resume t))
