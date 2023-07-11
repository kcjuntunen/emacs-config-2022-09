(add-to-list 'org-capture-templates
						 '("i" "Interuption" entry
							 (file kc/interruption-file)
							 "* %^{Heading} %T
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:ClientAccount: %^{ClientAccount}
:TradingPartnerAccount: %^{TradingPartnerAccount}
:END:

%?

:CLIPBOARD:
#+caption: Clipboard Content
#+begin_src text
%x
#+end_src
:END:"
							 :clock-in t
							 :clock-resume t))
