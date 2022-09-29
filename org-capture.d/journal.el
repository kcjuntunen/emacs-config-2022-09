(add-to-list 'org-capture-templates
						 '("j" "Journal" entry
							 (file+olp+datetree kc/diary-file)
							 "* %^{Heading} %T
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:Category: %^{Entry type|Bible|Note|Journal}
:END:
%?"
							 :clock-in t
							 :clock-resume t))
