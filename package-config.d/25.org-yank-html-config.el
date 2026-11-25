;;; 25.org-yank-html-config.el --- Paste clipboard HTML as Org.  -*- lexical-binding: t; -*-

;;; Commentary:

;; In an Org buffer, `C-c C-x y' (`yank-media') converts HTML on the clipboard
;; (e.g. copied from a web page) to Org with pandoc and inserts it.
;; Plain `C-y' still pastes plain text.

;;; Code:

(defun kc/org--html-fragment (html)
	"Return the copied part of Windows clipboard HTML.
The clipboard's \"HTML Format\" wraps the selection in a header and a
full document, and marks the selection with <!--StartFragment--> and
<!--EndFragment-->.  Without the markers, return HTML from its first
tag on.  Excel puts the markers inside <table>, around just the rows;
those are wrapped back in a table."
	(let* ((start-tag "<!--StartFragment-->")
				 (start (string-search start-tag html))
				 (end (and start (string-search "<!--EndFragment-->" html start)))
				 (case-fold-search t))
		(if (not end)
				(substring html (or (string-search "<" html) 0))
			(let ((fragment (substring html (+ start (length start-tag)) end)))
				(if (and (string-match-p "<t[rd][ >]" fragment)
								 (not (string-match-p "<table[ >]" fragment)))
						(concat "<table>" fragment "</table>")
					fragment)))))

(defun kc/org--yank-media-prefer-html (mimetypes)
	"Choose text/html from MIMETYPES when it holds more than an image.
For `yank-media-preferred-types'.  Excel and Word also offer a picture
of the selection, which `yank-media' would otherwise prefer.  HTML that
is only an <img> (a copied image) is left to the image handler."
	(when (memq 'text/html mimetypes)
		(let ((html (gui-get-selection 'CLIPBOARD 'text/html)))
			(when (and html
								 (string-match-p
									"[^[:space:]]"
									(replace-regexp-in-string
									 "<[^>]*>\\|&nbsp;" ""
									 (kc/org--html-fragment
										(if (multibyte-string-p html)
												html
											(decode-coding-string html 'utf-8))))))
				'(text/html)))))

(defun kc/org-html-to-org (html)
	"Convert the string HTML to Org text with pandoc."
	(with-temp-buffer
		(insert html)
		(let* ((coding-system-for-write 'utf-8-unix)
					 (coding-system-for-read 'utf-8-unix)
					 (status (call-process-region
										(point-min) (point-max) "pandoc" t t nil
										"--from=html-native_divs-native_spans-auto_identifiers"
										"--to=org" "--wrap=none" "--eol=lf")))
			(unless (eql status 0)
				(error "pandoc failed (%s): %s" status (buffer-string)))
			(string-trim (buffer-string)))))

(defun kc/org--clipboard-html-to-org (data)
	"Convert raw clipboard HTML DATA to Org text."
	(kc/org-html-to-org
	 (kc/org--html-fragment
		(if (multibyte-string-p data)
				data
			(decode-coding-string data 'utf-8)))))

(defun kc/org--demote-headings (org level)
	"Shift the headings in the Org text ORG so the highest is at LEVEL."
	(with-temp-buffer
		(insert org)
		(let ((top nil))
			(goto-char (point-min))
			(while (re-search-forward "^\\(\\*+\\) " nil t)
				(setq top (min (or top most-positive-fixnum)
											 (length (match-string 1)))))
			(when top
				(goto-char (point-min))
				(while (re-search-forward "^\\(\\*+\\) " nil t)
					(replace-match (make-string (+ (length (match-string 1)) (- level top)) ?*)
												 t t nil 1))))
		(buffer-string)))

(defun kc/org-clipboard-as-org (&optional level)
	"Return the clipboard as Org text, for a capture template's %(...).
HTML is converted with pandoc, and its headings shifted so the
highest is at LEVEL (default 3).  Plain text is returned in a text
src block.  An empty clipboard gives \"\"."
	(let ((html (gui-get-selection 'CLIPBOARD 'text/html))
				(text (gui-get-selection 'CLIPBOARD 'UTF8_STRING)))
		(cond
		 (html
			(kc/org--demote-headings (kc/org--clipboard-html-to-org html) (or level 3)))
		 ((and text (not (string-empty-p text)))
			(concat "#+begin_src text\n"
							(org-escape-code-in-string (substring-no-properties text))
							(if (string-suffix-p "\n" text) "" "\n")
							"#+end_src"))
		 (t ""))))

(defun kc/org-yank-html (_mimetype data)
	"Insert clipboard HTML DATA as Org.
A `yank-media' handler for text/html."
	(insert (kc/org--clipboard-html-to-org data) "\n"))

(defun kc/org-register-yank-html ()
	"Let `yank-media' paste HTML into this Org buffer."
	(when (fboundp 'yank-media-handler)
		(yank-media-handler "text/html" #'kc/org-yank-html)
		(setq-local yank-media-preferred-types
								(cons #'kc/org--yank-media-prefer-html
											(default-value 'yank-media-preferred-types)))))

(add-hook 'org-mode-hook #'kc/org-register-yank-html)

(with-eval-after-load 'org
	(define-key org-mode-map (kbd "C-c C-x y") #'yank-media))

(provide 'org-yank-html-config)
;;; 25.org-yank-html-config.el ends here
