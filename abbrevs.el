;;; abbrevs.el -*- lexical-binding: t; -*-

(define-abbrev-table 'global-abbrev-table
	'(("rrr" "→")
		("lll" "←")
		("uuu" "↑")
		("ddd" "↓")
		("tu" "👍")
		("td" "👎")
		("chm" "✓")
		("MSD" "Mid-States")
		("RK" "Rural King")
		("xref" "cross reference")
		("xdock" "cross dock")))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)
