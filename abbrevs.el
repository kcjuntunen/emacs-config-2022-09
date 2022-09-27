;;; abbrevs.el -*- lexical-binding: t; -*-

(define-abbrev-table 'global-abbrev-table
  '(("rrr" "→")
    ("lll" "←")
    ("tu" "👍")
    ("td" "👎")
		("chm", "✓")))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)
