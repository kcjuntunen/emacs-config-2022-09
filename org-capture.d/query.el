(if (not at-work)
		(message "Not adding query capture template.")
	(add-to-list 'org-capture-templates
							 '("q" "Query" entry
								 (file kc/interruption-file)
								 "* %^{Heading} [%(churchhill-datetime)]
:PROPERTIES:
:CUSTOM_ID: %(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:Captured: %U
:EXPORT_FILE_NAME: c:/fastrack/workarea/%(time-stamp--format \"%Y%m%d%H%M\" (org-read-date nil t \"+0d\"))
:header-args:sql: :engine mysql
:header-args:sql+: :dbhost \"%^{Host}\"
:header-args:sql+: :dbuser (kc/auth-get-key \"yorkdb\" :user)
:header-args:sql+: :dbpassword (kc/auth-get-pwd \"yorkdb\")
:header-args:sql+: :database \"%^{Database}\"
:header-args:sql+: :exports results
:header-args:sql+: :exports results
:header-args:sql+: :cmdline "--skip-ssl"
:END:


#+caption: Query
#+begin_src sql
%?
#+end_src"
								 :clock-in t
								 :clock-resume t)))
