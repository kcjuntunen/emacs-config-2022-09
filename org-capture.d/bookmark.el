(add-to-list 'org-capture-templates
						 '("b" "Bookmark" entry
							 (file+headline kc/personal-notes-file "Bookmarks")
							 "* %^{Heading}
:PROPERTIES:
:Captured: %U
:END:

%?"
							 :empty-lines 1))
