(defvar at-work (or (string-equal (downcase (system-name)) "pascal")
										(getenv "WSL_DISTRO_NAME"))
	"Smyrno is the machine at work.")
