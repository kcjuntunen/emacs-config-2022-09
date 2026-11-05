;;; 21.password-generator.el --- Generate passwords  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  K. C. Juntunen

;; Author: K. C. Juntunen <k.c.juntunen@yorkwwt.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; M-x kc/password copies a new password to the kill ring (and so the
;; clipboard) without displaying it.  A numeric prefix sets the length,
;; e.g. C-u 30 M-x kc/password.
;;
;; Rules enforced:
;;   - 12 to 40 characters
;;   - at least one lowercase letter and one digit
;;   - doesn't contain the first name, last name or user name
;;     (case-insensitive); see `kc/password-first-name' and friends.
;;
;; Randomness comes from the operating system's secure generator
;; (/dev/urandom, or .NET's RandomNumberGenerator through PowerShell
;; on Windows), not Emacs's `random'.

;;; Code:

(require 'seq)

(defvar kc/password-min-length 12 "Shortest password allowed.")
(defvar kc/password-max-length 40 "Longest password allowed.")
(defvar kc/password-default-length 20 "Length used without a prefix argument.")

(defvar kc/password-first-name (car (split-string user-full-name))
	"First name the password must not contain.")

(defvar kc/password-last-name (car (last (split-string user-full-name)))
	"Last name the password must not contain.")

(defvar kc/password-user-name user-login-name
	"User name the password must not contain.")

(defvar kc/password-characters
	(concat "abcdefghijkmnopqrstuvwxyz"   ; no l
					"ABCDEFGHJKLMNPQRSTUVWXYZ"    ; no I, O
					"23456789"                    ; no 0, 1
					"!#$%*+-=?@^_")
	"Characters passwords are drawn from.
Look-alikes (l/I/1, O/0) are left out so passwords can be read aloud
or retyped.")

(defun kc/password--random-bytes (n)
	"Return a list of N cryptographically random bytes from the OS."
	(cond
	 ((file-readable-p "/dev/urandom")
		(with-temp-buffer
			(set-buffer-multibyte nil)
			(insert-file-contents-literally "/dev/urandom" nil 0 n)
			(string-to-list (buffer-string))))
	 ((eq system-type 'windows-nt)
		(let ((hex (with-output-to-string
								 (with-current-buffer standard-output
									 (unless (zerop (call-process
																	 "powershell" nil t nil "-NoProfile" "-NonInteractive" "-Command"
																	 (format "$b = New-Object byte[] %d; [Security.Cryptography.RandomNumberGenerator]::Create().GetBytes($b); [BitConverter]::ToString($b)" n)))
										 (error "PowerShell failed to produce random bytes"))))))
			(mapcar (lambda (h) (string-to-number h 16))
							(split-string (string-trim hex) "-" t))))
	 (t (error "No secure random source available"))))

(defun kc/password--random-string (length chars)
	"Return LENGTH characters picked uniformly at random from CHARS."
	(let* ((n (length chars))
				 ;; Bytes at or above LIMIT are discarded so every character is
				 ;; equally likely (no modulo bias).
				 (limit (- 256 (% 256 n)))
				 (bytes nil)
				 (result nil))
		(while (< (length result) length)
			(unless bytes
				(setq bytes (kc/password--random-bytes (* 2 length))))
			(let ((b (pop bytes)))
				(when (< b limit)
					(push (aref chars (% b n)) result))))
		(concat result)))

(defun kc/password-valid-p (password &optional names)
	"Return non-nil if PASSWORD follows the rules.
NAMES are strings it must not contain, ignoring case; empty ones are
skipped."
	(let ((case-fold-search nil))
		(and (<= kc/password-min-length (length password) kc/password-max-length)
				 (string-match-p "[a-z]" password)
				 (string-match-p "[0-9]" password)
				 (not (seq-some (lambda (name)
													(and name (not (string-empty-p name))
															 (string-search (downcase name) (downcase password))))
												names)))))

(defun kc/generate-password (&optional length)
	"Return a new password of LENGTH characters that follows the rules."
	(let ((length (or length kc/password-default-length))
				(names (list kc/password-first-name kc/password-last-name kc/password-user-name))
				(tries 0)
				password)
		(unless (<= kc/password-min-length length kc/password-max-length)
			(user-error "Length must be between %d and %d"
									kc/password-min-length kc/password-max-length))
		(while (not (and password (kc/password-valid-p password names)))
			(when (> (setq tries (1+ tries)) 100)
				(error "Couldn't generate a valid password in 100 tries"))
			(setq password (kc/password--random-string length kc/password-characters)))
		password))

(defun kc/password (&optional length)
	"Copy a new password to the kill ring without showing it.
With a numeric prefix argument, make it LENGTH characters long."
	(interactive "P")
	(let ((password (kc/generate-password (and length (prefix-numeric-value length)))))
		(kill-new password)
		(message "Copied a %d-character password to the clipboard" (length password))))

(provide '21.password-generator)
;;; 21.password-generator.el ends here
