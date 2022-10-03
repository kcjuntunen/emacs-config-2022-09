;;; package-config.el --- set up package system   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  K. C. Juntunen

;; Author: K. C. Juntunen <k.c.juntunen@yorkwwt.com>
;; Keywords: lisp

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

;; 

;;; Code:
(require 'package)

(unless (assoc-default "melpa" package-archives)
	(add-to-list 'package-archives
							 '("melpa" . "https://stable.melpa.org/packages/") t))
(unless (assoc-default "nongnu" package-archives)
	(add-to-list 'package-archives
							 '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))


;;; from purcell/emacs.d
(defun require-package (package &optional min-version no-refresh)
	"Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
	(if (package-installed-p package min-version)
			t
		(if (or (assoc package package-archive-contents) no-refresh)
				(package-install package)
			(progn
				(package-refresh-contents)
				(require-package package min-version t)))))

(package-initialize)

(provide 'package-config)
;;; package-config.el ends here
