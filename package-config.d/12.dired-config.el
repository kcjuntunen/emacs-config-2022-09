(require 'dired)
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

(add-hook #'dired-mode-hook
					(lambda ()
						(dired-hide-details-mode)
						(hl-line-mode 1)
						(text-scale-adjust -1)
						(if (or (string-equal (buffer-name) "workarea")
										(string-equal (buffer-name) "fastrack"))
								(auto-revert-mode 1)
							(message "Buffer name is %s" (buffer-name)))))

(if (not (string-equal system-type "darwin"))
		(setq dired-listing-switches "-alhGF --dired --group-directories-first")
	(setq dired-listing-switches "-alh"))
;; (setq dired-listing-switches "-alF --group-directories-first")

(provide 'dired-config)
