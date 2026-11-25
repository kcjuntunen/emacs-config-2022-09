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
tag on."
	(if (string-match "<!--StartFragment-->\\(\\(?:.\\|\n\\)*\\)<!--EndFragment-->" html)
			(match-string 1 html)
		(substring html (or (string-search "<" html) 0))))

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

(defun kc/org-yank-html (_mimetype data)
	"Insert clipboard HTML DATA as Org.
A `yank-media' handler for text/html."
	(let ((html (kc/org--html-fragment
							 (if (multibyte-string-p data)
									 data
								 (decode-coding-string data 'utf-8)))))
		(insert (kc/org-html-to-org html) "\n")))

(defun kc/org-register-yank-html ()
	"Let `yank-media' paste HTML into this Org buffer."
	(when (fboundp 'yank-media-handler)
		(yank-media-handler "text/html" #'kc/org-yank-html)))

(add-hook 'org-mode-hook #'kc/org-register-yank-html)

(with-eval-after-load 'org
	(define-key org-mode-map (kbd "C-c C-x y") #'yank-media))

(provide 'org-yank-html-config)
;;; 25.org-yank-html-config.el ends here
