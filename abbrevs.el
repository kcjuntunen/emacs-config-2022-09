;;; abbrevs.el -*- lexical-binding: t; -*-

(define-abbrev-table 'global-abbrev-table
	'(("rrr" "→")
		("lll" "←")
		("tu" "👍")
		("td" "👎")
		("chm" "✓")
		("M-S" "Mid-States")))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)
