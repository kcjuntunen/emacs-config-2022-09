;; -*- lexical-binding: t; -*-
(if (not at-work)
		(message "Not adding interruption capture template.")
	(add-to-list 'org-capture-templates
							 '("i" "Interuption" entry
								 (file kc/interruption-file)
								 "* %^{Heading} [%(churchhill-datetime)]
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:ClientAccount: %^{ClientAccount}
:TradingPartnerAccount: %^{TradingPartnerAccount}
:EXPORT_FILE_NAME: c:/fastrack/workarea/%(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:END:

%?

** Clipboard Contents
%(kc/org-clipboard-as-org 3)"
								 :clock-in t
								 :clock-resume t)))
