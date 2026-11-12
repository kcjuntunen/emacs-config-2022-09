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

;; There's no zip, unzip or 7z on Windows, but the bundled bsdtar can
;; read and write both formats (-a picks the format from the suffix).
;; Use its full path, because Git's GNU tar comes first on `exec-path'
;; and can't.  When extracting, %o is a folder named after the archive;
;; bsdtar won't create it, so cmd's mkdir does (quietly, if it exists).
(when (eq system-type 'windows-nt)
	(with-eval-after-load 'dired-aux
		(let* ((bsdtar (shell-quote-argument
										(expand-file-name "System32/tar.exe" (getenv "SystemRoot"))))
					 (pack (concat bsdtar " -a -cf %o %i"))
					 (unpack (concat "mkdir %o 2>nul & " bsdtar " -xf %i -C %o")))
			(dolist (re '("\\.zip\\'" "\\.7z\\'"))
				(setf (alist-get re dired-compress-files-alist nil nil #'equal) pack)
				(setf (alist-get re dired-compress-file-suffixes nil nil #'equal)
							(list "" unpack))))))

(provide 'dired-config)
