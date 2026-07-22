(require 'dired)

;; (if (eq system-type 'windows-nt)
;; 		(message "dired-async is broken in Windows")
;; 	(autoload 'dired-async-mode "dired-async.el" nil t)
;; 	(dired-async-mode 1))

(add-hook 'dired-mode-hook
					(lambda ()
						(dired-hide-details-mode)
						;; (hl-line-mode 1)
						(text-scale-adjust -1)
						(if (or (string-equal (buffer-name) "workarea")
										(string-equal (buffer-name) "fastrack"))
								(auto-revert-mode 1))))

(if (not (string-equal system-type "darwin"))
		(setq dired-listing-switches "-alhGF --dired --group-directories-first")
	(setq dired-listing-switches "-alh"))
;; (setq dired-listing-switches "-alF --group-directories-first")

(if (not (getenv "WSL_DISTRO_NAME"))
		(message "Skipping WSL stuff.")
	;; start apps with windows under WSL
	(defun kc/wsl-open-with-windows (file)
		"Open FILE using Windows default application from WSL."
		(interactive (list (dired-get-filename)))
		(let ((winpath (shell-command-to-string
										(format "wslpath -w %s" (shell-quote-argument file)))))
			(shell-command (format "cmd.exe /c start \"\" %s" (string-replace "\\" "\\\\" winpath)))))

	(with-eval-after-load 'dired
		(define-key dired-mode-map (kbd "W") #'kc/wsl-open-with-windows)))

(provide 'dired-config)
