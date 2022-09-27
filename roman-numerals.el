;;; roman-numerals.el --- roman-numerals Exercise (exercism)

;;; Commentary:
;;; Comment

;;; Code:
(require 'cl-lib)
(defvar arabic-to-numeral '((1000 . "M")
														(900  . "CM")
														(500  . "D")
														(400  . "CD")
														(100  . "C")
														(90   . "XC")
														(50   . "L")
														(40   . "XL")
														(10   . "X")
														(9    . "IX")
														(5    . "V")
														(4    . "IV")
														(1    . "I"))
	"List of int to Roman numeral pairs.")

(defun to-roman (n)
	"Convert N to Roman numerals."
	(with-temp-buffer
		(cl-loop for i in arabic-to-numeral do
						 (let ((count (/ n (car i)))
									 (res (cdr i)))
							 (dotimes (j count) (insert res)
												(setq n (- n (car i))))))
		(buffer-string)))

(defun roman-month ()
	"The month in Roman numerals."
	(to-roman (nth 4 (decode-time (current-time)))))

(defun roman-year ()
	"The year in Roman numerals."
	(to-roman (nth 5 (decode-time (current-time)))))

(defun churchhill-date ()
	"The style of date I saw in `The Gathering Storm'."
	(format-time-string
	 (format "%%d.%s.%%y"
					 (roman-month))))

(defun cool-date ()
	"Big, long old-timey style date."
	(let* ((dom (nth 3 (decode-time (current-time))))
				 (ordinal (cond ((< (mod dom 10) 2) "st")
												((< (mod dom 10) 3) "nd")
												((< (mod dom 10) 4) "rd")
												(t "th"))))
		(format-time-string
		 (format "%%A, the %%d%s of %%B, the year of our Lord %s" ordinal
						 (roman-year)))))

(provide 'roman-numerals)
;;; roman-numerals.el ends here
