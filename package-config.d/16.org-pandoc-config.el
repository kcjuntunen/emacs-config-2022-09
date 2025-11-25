;; (require-package 'ox-pandoc)
(setq org-pandoc-menu-entry
			'((?4 "to html5 and open." org-pandoc-export-to-html5-and-open)
				(?$ "as html5." org-pandoc-export-as-html5)
				(?5 "to html5-pdf and open." org-pandoc-export-to-html5-pdf-and-open)
				(?% "to html5-pdf." org-pandoc-export-to-html5-pdf)
				(?u "to dokuwiki and open." org-pandoc-export-to-dokuwiki-and-open)
				(?U "as dokuwiki." org-pandoc-export-as-dokuwiki)
				(?x "to docx and open." org-pandoc-export-to-docx-and-open)
				(?X "to docx." org-pandoc-export-to-docx)
				(?m "as Markdown." org-pandoc-export-as-markdown)
				(?M "to Markdown." org-pandoc-export-to-markdown)))

(provide 'pandoc-config)
