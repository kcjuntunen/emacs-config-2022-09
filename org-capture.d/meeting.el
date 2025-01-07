(add-to-list 'org-capture-templates
						 '("m" "Meeting" entry
							 (file kc/meetings-file)
							 "* MEETING %^{Heading} [%(churchhill-datetime)]
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Request: Not billable
:ClientAccount: %^{ClientAccount}
:TradingPartnerAccount: n/a
:Captured: %U
:END:
%?"
							 :clock-in t
							 :clock-resume t))
