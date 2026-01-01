;;; helpful-config.el --- Config for helpful package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  K. C. Juntunen

;; Author: K. C. Juntunen <juntunenkc@falken>
;; Keywords: 

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
(require-package 'helpful)
(require 'helpful)

(setq-default
 counsel-describe-function-function #'helpful-callable
 counsel-describe-variable-function #'helpful-variable)

(define-key global-map [remap describe-command] 'helpful-command)
(define-key global-map [remap describe-key] 'helpful-key)

(provide 'helpful-config)
;;; helpful-config.el ends here
