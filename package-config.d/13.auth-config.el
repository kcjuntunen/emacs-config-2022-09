;;; -*- lexical-binding: t; -*-

;; Credentials live only in the encrypted ~/.authinfo.gpg.  The portable
;; GnuPG has just a console pinentry, so have Emacs ask for the passphrase
;; in the minibuffer instead.
(require 'epg)
(setq epg-pinentry-mode 'loopback
			auth-sources '("~/.authinfo.gpg"))

;; On Windows, gpg never sees EOF when epg pipes the plaintext to its stdin,
;; so saving a .gpg file hangs until C-g.  Encrypt from a temp file instead,
;; the way epg already does when signing.
(defun kc/epg-encrypt-string-via-file (orig context plain recipients
																						&optional sign always-trust)
	"Call ORIG unless on Windows; there, encrypt PLAIN via a temp file."
	(if (not (eq system-type 'windows-nt))
			(funcall orig context plain recipients sign always-trust)
		(let ((input (make-temp-file "epg-input")))
			(unwind-protect
					(progn
						(let ((coding-system-for-write 'binary))
							(write-region plain nil input nil 'silent))
						(epg-encrypt-file context input recipients nil sign always-trust))
				(delete-file input)))))
(advice-add 'epg-encrypt-string :around #'kc/epg-encrypt-string-via-file)

;; from <https://magnus.therning.org/2024-09-01-improving-how-i-handle-secrets-in-my-work-notes.html>
(require 'dash)
(defun kc/auth-get-pwd (host)
  "Get the password for a host (authinfo.gpg)"
  (-> (auth-source-search :host host)
      car
      (plist-get :secret)
      funcall))

(defun kc/auth-get-key (host key)
  "Get a key's value for a host (authinfo.gpg)

Not usable for getting the password (:secret), use 'kc/auth-get-pwd'
for that."
  (-> (auth-source-search :host host)
      car
      (plist-get key)))

(provide 'auth-config)
