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

;;; Zip and 7z on Windows

;; Emacs can't pass non-ASCII names on a Windows command line intact, so
;; with 7-Zip no file name goes on it: `c' hands 7za the marked files in
;; a UTF-8 list file and writes the archive under a temporary ASCII name,
;; and `Z' passes the 8.3 short names of the archive and the folder.

(defvar kc/7za-program "C:/Users/k.c.juntunen/opt/7z2606-extra/x64/7za.exe"
	"Standalone 7-Zip, used by Dired for .zip and .7z files.")

(defconst kc/7za-archive-regexp "\\.\\(?:7z\\|zip\\)\\'")

(defun kc/7za-usable-p (file)
	(and (eq system-type 'windows-nt)
			 (file-executable-p kc/7za-program)
			 (not (file-remote-p file))
			 (let (case-fold-search) (string-match-p kc/7za-archive-regexp file))))

(defun kc/7za-short-name (file)
	(or (w32-short-file-name file) file))

(defun kc/7za-run (dir &rest args)
	"Run 7za in DIR with ARGS; signal an error showing its output on failure."
	(with-current-buffer (get-buffer-create " *7za output*")
		(erase-buffer)
		(let* ((default-directory (file-name-as-directory (kc/7za-short-name dir)))
					 (status (apply #'call-process kc/7za-program nil t nil
													"-bso0" "-bsp0" "-y" args)))
			(unless (eql status 0)
				(error "7za failed (%s): %s" status (string-trim (buffer-string)))))))

(defun kc/dired-7za-compress-to (orig)
	"Make .zip and .7z archives with 7za; leave other formats to ORIG."
	(let* ((in-files (dired-get-marked-files nil nil nil nil t))
				 (out-file (expand-file-name (read-file-name "Compress to: "))))
		(if (not (kc/7za-usable-p out-file))
				(cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) out-file)))
					(funcall orig))
			(when (or (not (file-exists-p out-file))
								(y-or-n-p (format "%s exists, overwrite?"
																	(abbreviate-file-name out-file))))
				(let* ((out-dir (file-name-directory out-file))
							 (tmp (expand-file-name (concat (make-temp-name "kc-7za-")
																							(file-name-extension out-file t))
																			(kc/7za-short-name out-dir)))
							 (list-file (make-temp-file "kc-7za-" nil ".txt")))
					(unwind-protect
							(progn
								(let ((coding-system-for-write 'utf-8-unix))
									(write-region (mapconcat #'file-relative-name in-files "\n")
																nil list-file nil 'silent))
								(kc/7za-run default-directory "a" "-scsUTF-8" tmp
														(concat "@" (kc/7za-short-name list-file)))
								(rename-file tmp out-file t))
						(delete-file list-file)
						(when (file-exists-p tmp) (delete-file tmp)))
					(dired-add-file out-file)
					(message (ngettext "Compressed %d file to %s" "Compressed %d files to %s"
														 (length in-files))
									 (length in-files) (file-name-nondirectory out-file))))
			(dired-post-do-command))))

(defun kc/dired-7za-compress-file (orig file)
	"Extract .zip and .7z archives with 7za; leave other files to ORIG."
	(if (not (kc/7za-usable-p file))
			(funcall orig file)
		(let ((out-dir (file-name-as-directory (file-name-sans-extension file))))
			(make-directory out-dir t)
			(kc/7za-run (file-name-directory file) "x" "-aoa"
									(concat "-o" (kc/7za-short-name out-dir))
									(kc/7za-short-name file))
			out-dir)))

;; Without 7-Zip, fall back to Windows' bundled bsdtar, which reads and
;; writes both formats (-a picks the format from the suffix) but crashes
;; on some names and only gets names in the ANSI code page.  Use its full
;; path, because Git's GNU tar comes first on `exec-path' and can't.
;; When extracting, %o is a folder named after the archive; bsdtar won't
;; create it, so cmd's mkdir does (quietly, if it exists).
(when (eq system-type 'windows-nt)
	(with-eval-after-load 'dired-aux
		(advice-add 'dired-do-compress-to :around #'kc/dired-7za-compress-to)
		(advice-add 'dired-compress-file :around #'kc/dired-7za-compress-file)
		(let* ((bsdtar (shell-quote-argument
										(expand-file-name "System32/tar.exe" (getenv "SystemRoot"))))
					 (pack (concat bsdtar " -a -cf %o %i"))
					 (unpack (concat "mkdir %o 2>nul & " bsdtar " -xf %i -C %o")))
			(dolist (re '("\\.zip\\'" "\\.7z\\'"))
				(setf (alist-get re dired-compress-files-alist nil nil #'equal) pack)
				(setf (alist-get re dired-compress-file-suffixes nil nil #'equal)
							(list "" unpack))))))

(provide 'dired-config)
