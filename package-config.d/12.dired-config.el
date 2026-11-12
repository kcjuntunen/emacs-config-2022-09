(require 'dired)

(if (eq system-type 'windows-nt)
		(message "dired-async is broken in Windows")
	(autoload 'dired-async-mode "dired-async.el" nil t)
	(dired-async-mode 1))

(add-hook 'dired-mode-hook
					(lambda ()
						(dired-hide-details-mode)
						;; (hl-line-mode 1)
						(text-scale-adjust -1)
						(if (not (or (string-equal (buffer-name) "workarea")
										(string-equal (buffer-name) "fastrack")))
								t
							(auto-revert-mode 1)
							(dired-sort-toggle))))

(if (not (string-equal system-type "darwin"))
		(setq dired-listing-switches "-alhGF --dired --group-directories-first")
	(setq dired-listing-switches "-alh"))
;; (setq dired-listing-switches "-alF --group-directories-first")

;; There's no zip.exe or 7z.exe on Windows, but the bundled bsdtar can
;; write both (-a picks the format from the suffix).  Use its full path,
;; because Git's GNU tar comes first on `exec-path' and can't.
(when (eq system-type 'windows-nt)
	(with-eval-after-load 'dired-aux
		(let ((bsdtar (concat (shell-quote-argument
													 (expand-file-name "System32/tar.exe" (getenv "SystemRoot")))
													" -a -cf %o %i")))
			(setf (alist-get "\\.zip\\'" dired-compress-files-alist nil nil #'equal) bsdtar)
			(setf (alist-get "\\.7z\\'" dired-compress-files-alist nil nil #'equal) bsdtar))))

(provide 'dired-config)
