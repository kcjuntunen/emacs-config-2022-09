;;; cobol-mode-config.el --- config for COBOL mode  -*- lexical-binding: t; -*-

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
(when at-work
	(require-package 'cobol-mode)
	(require 'cobol-mode)
	(add-to-list 'auto-mode-alist '("\\.cbl\\'" . cobol-mode))
	(add-to-list 'auto-mode-alist '("\\.cpy\\'" . cobol-mode))
	(add-hook 'cobol-mode-hook 'ruler-mode))

(provide 'cobol-mode-config)
;;; cobol-mode-config.el ends here
