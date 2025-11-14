(add-to-list 'org-capture-templates
						 '("q" "Query" entry
							 (file kc/interruption-file)
							 "* %^{Heading} [%(churchhill-datetime)]
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:END:



:CLIPBOARD:
#+caption: Query
#+begin_src sql :engine \"mysql\" :dbhost \"%^{Host}\" :database \"%^{Database}\" :exports results
%?%x
#+end_src

#+RESULTS: Output
:END:"
							 :clock-in t
							 :clock-resume t))
