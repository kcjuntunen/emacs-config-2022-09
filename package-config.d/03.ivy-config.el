;;; ivy-config.el --- ivy-config                     -*- lexical-binding: t; -*-

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
(require-package 'ivy)
(require 'ivy)
(ivy-mode 1)

;; Emacs 31's `cl-remove-if-not' calls PREDICATE even when it's nil
;; (Emacs 30 kept everything), and ivy 0.15.1 passes nil whenever a
;; caller like `org-set-property' gives `completing-read' a list of
;; lists and no predicate: (void-function nil).  Fixed in ivy's master,
;; so drop this once ELPA has a release newer than 0.15.1.
(defun kc/cl-remove-if-not-nil-predicate (orig pred seq &rest keys)
	"Call ORIG with PRED, SEQ and KEYS, but keep all of SEQ if PRED is nil."
	(if pred (apply orig pred seq keys) seq))
(when (version<= "31" emacs-version)
	(advice-add 'cl-remove-if-not :around #'kc/cl-remove-if-not-nil-predicate))



(provide 'ivy-config)
;;; ivy-config.el ends here
