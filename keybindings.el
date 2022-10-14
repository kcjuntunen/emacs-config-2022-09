;;; keybindings.el --- Keybindings mostly in one place.  -*- lexical-binding: t; -*-

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
(define-key global-map (kbd "C-<f5>") 'org-agenda)
(define-key global-map (kbd "<f5>") 'kc/org-check-agenda)
(define-key global-map (kbd "<f6>") 'org-capture)
(define-key global-map (kbd "C-x c") 'calc)
(define-key global-map (kbd "C-x E") 'eshell)
(define-key global-map (kbd "C-x p") 'proced)
(define-key dired-mode-map (kbd "C-c C-w") 'wdired-change-to-wdired-mode)

;; I know these keys have handy default functions, but they just confuse me.
(add-hook 'org-mode-hook
					#'(lambda ()
							(define-key org-agenda-mode-map (kbd "h") 'backward-char)
							(define-key org-agenda-mode-map (kbd "l") 'forward-char)
							(define-key org-agenda-mode-map (kbd "j") 'next-line)
							(define-key org-agenda-mode-map (kbd "k") 'previous-line)))

(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
(evil-define-key 'normal helpful-mode-map (kbd "q") #'quit-window)

(if (version< "28" emacs-version)
		(repeat-mode 1)
	(message "repeat-mode is not supported by emacs version %s" emacs-version))

(provide 'keybindings)
;;; keybindings.el ends here
