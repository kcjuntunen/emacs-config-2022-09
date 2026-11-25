;;; 24.org-transclusion-config.el --- Set up org-transclusion  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  K. C. Juntunen

;; Author: K. C. Juntunen <k.c.juntunen@yorkwwt.com>
;; Keywords: outlines, hypermedia

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

;; org-transclusion (GNU ELPA) shows the content of linked Org files,
;; headings, or line ranges inline via #+transclude: keywords.
;;
;; C-c C-x t  org-transclusion-add (transclude the keyword at point)
;; C-c C-x T  org-transclusion-mode (toggle all transclusions in buffer)

;;; Code:

(use-package org-transclusion
	:ensure t
	:after org
	:bind (:map org-mode-map
					("C-c C-x t" . org-transclusion-add)
					("C-c C-x T" . org-transclusion-mode))
	:config
	(setq org-transclusion-exclude-elements nil))

(provide '24.org-transclusion-config)
;;; 24.org-transclusion-config.el ends here
