(add-to-list 'org-capture-templates
						 '("t" "todo" entry
							 (file kc/refile-file)
							 "* TODO ^{Heading}
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:ClientAccount: %^{ClientAccount}
:TradingPartnerAccount: %^{TradingPartnerAccount}
:END:

%?"
							 :clock-in t
							 :clock-resume t
							 :prepend t))
