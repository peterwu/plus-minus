;;; plus-minus.el --- plus/minus things  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Peter Wu
;; Author: Peter Wu <peterwu@hotmail.com>
;; URL: http://github.com/peterwu/plus-minus

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements plus-minus operations on the following number
;; formats:

;; - binary numbers with leading 0b or 0B
;; - octal numbers with leading 0
;; - hexadecimal numbers with leading 0x or 0X
;; - decimal numbers

;; It is designed with extensibility in mind and new patterns, such as
;; datetime, can be later added.

;; It works like C-a/C-x in Vim, i.e. searches for number(s) till the end of
;; line and then increments or decrements based on context.

;; Also, Vim’s g_CTRL-A and g_CTRL-X are supported on blocks (rectangles).
;; If a region is marked, then all the numbers within the region can be
;; plus-minus’d.

;; Install:

;; (use-package plus-minus
;;   :ensure t
;;   :bind (("C-c C-a"   . +/-:forward+)
;;	    ("C-c C-x"   . +/-:forward-)
;;	    ("C-c M-a"   . +/-:backward+)
;;	    ("C-c M-x"   . +/-:backward-)
;;	    ("C-c g C-a" . +/-:block+)
;;	    ("C-c g C-x" . +/-:block-)))

;; Evil users may want to use your leader key in the place of "C-c".

;; This package supports forward and backward searches and allows for increments
;; other than the default value, 1.
;; For example, with the above key binding:
;; `C-u 5 C-c C-a’ will let you increment by 5

;;; Code:

(require 'rx)

(defgroup plus-minus ()
  "plus/minus items."
  :group 'lisp
  :prefix "+/-")

(defcustom +/-:step +1
  "Define increment value.
A positive number means increment; while a negative number means decrement."
  :type 'number)

(defcustom +/-:forward? +1
  "Define default search direction. +1 stands for forward while -1 backward."
  :type 'number)

(defvar +/-:match-handle-alist
  "Define a list of supported match patterns and their respective handlers. This
list may be updated to support future plus-minus’able items."
  '((+/-:match-binary      . +/-:handle-binary)
    (+/-:match-octal       . +/-:handle-octal)
    (+/-:match-hexadecimal . +/-:handle-hexadecimal)
    (+/-:match-decimal     . +/-:handle-decimal)))

(defvar +/-:match-binary
  "Define the regular expression for binary numbers."
  (rx (and ?0 (= 1 (in "bB"))
	   (one-or-more (in "01")))))

(defvar +/-:match-octal
  "Define the regular expression for octal numbers."
  (rx (and ?0 (one-or-more (in "0-7")))))

(defvar +/-:match-hexadecimal
  "Define the regular expression for hexadecimal numbers."
  (rx (and ?0 (= 1 (in "xX")) (one-or-more xdigit))))

(defvar +/-:match-decimal
  "Define the regular expression for decimal numbers."
  (rx (and (zero-or-one (or ?- ?+))
	   (one-or-more digit))))

(defun +/-:format-binary (number)
  "Format a decimal NUMBER into binary form."
  (let ((res (when (= number 0) "0")))
    (while (not (= number 0))
      (setq res (concat (number-to-string (% number 2)) res))
      (setq number (ash number -1)))
    (format "0b%s" res)))

(defun +/-:handle-binary (step)
  "Handle binary numbers in terms of plus/minus functionality with STEP amount
of increment. If the handling succeeds, return t; otherwise nil."
  (when (and (char-after)
	     (or (= (char-after) ?0)
		 (= (char-after) ?1)))
    (let ((point (point)))
      (skip-chars-backward "bB01")
      (if (looking-at +/-:match-binary)
	  (let ((result (+ step (string-to-number (substring(match-string 0) 2) 2))))
	    (if (< result 0) (setq result 0))
	    (replace-match (+/-:format-binary result))
	    (backward-char)
	    t)
	(goto-char point)
	nil))))

(defun +/-:handle-octal (step)
  "Handle octal numbers in terms of plus/minus functionality with STEP amount
of increment. If the handling succeeds, return t; otherwise nil."
  (when (and (char-after)
	     (>= (char-after) ?0)
	     (<= (char-after) ?7))
    (let ((point (point)))
      (skip-chars-backward "0-7")
      (if (looking-at +/-:match-octal)
	  (let ((result (+ step (string-to-number (match-string 0) 8))))
	    (if (< result 0) (setq result 0))
	    (replace-match
	     (format "0%o" result))
	    (backward-char)
	    t)
	(goto-char point)
	nil))))

(defun +/-:handle-hexadecimal (step)
  "Handle hexadecimal numbers in terms of plus/minus functionality with STEP amount
of increment. If the handling succeeds, return t; otherwise nil."
  (when (and (char-after)
	     (or (and (>= (char-after) ?0)
		      (<= (char-after) ?9))
		 (and (>= (char-after) ?A)
		      (<= (char-after) ?F))
		 (and (>= (char-after) ?a)
		      (<= (char-after) ?f))))
    (let ((point (point)))
      (skip-chars-backward "xX0-9A-Fa-f")
      (if (looking-at +/-:match-hexadecimal)
	  (let ((result (+ step (string-to-number (substring(match-string 0) 2) 16))))
	    (if (< result 0) (setq result 0))
	    (replace-match (format "0x%X" result ))
	    (backward-char)
	    t)
	(goto-char point)
	nil))))

(defun +/-:handle-decimal (step)
  "Handle decimal numbers in terms of plus/minus functionality with STEP amount
of increment. If the handling succeeds, return t; otherwise nil."
  (when (and (char-after)
	     (>= (char-after) ?0)
	     (<= (char-after) ?9))
    (let ((point (point)))
      (skip-chars-backward "0-9")
      (skip-chars-backward "+-")
      (if (looking-at +/-:match-decimal)
	  (let ((result (+ step (string-to-number (match-string 0) 10))))
	    (replace-match (format "%d" result))
	    (backward-char)
	    t)
	(goto-char point)
	nil))))

(defun +/-:search (forward? limit)
  "Search for items matching patterns until the LIMIT is reached. If FORWARDS? is
a positive number, then search forward; if negative, then search backward. If the
search attempt succeeds, return t; otherwise nil."
  (let ((match (rx (one-or-more digit))))
    (when (cond
	   ((> forward? 0)
	    (re-search-forward match limit t))
	   (t
	    (re-search-backward match (point-at-bol) t)
	    (re-search-forward match (point-at-eol) t)))
      (backward-char)
      t)))

(defun +/-:plus-minus-at-point (step)
  "Attempt to parse the item under point with STEP amount of increment.
If it succeeds, return t; otherwise nil."
  (let* ((alist +/-:match-handle-alist)
	 (handled? nil)
	 (point (point)))
    (while (and alist (not handled?))
      (let* ((elm (car alist))
	     (handle (cdr elm)))
	(setq handled? (funcall handle step))
	(setq alist (cdr alist))))
    handled?))

(defun +/-:plus-minus (forward? step limit)
  "Perform the plus-minus operation with STEP amount of increment until LIMIT is reached.

If FORWARDS? is positive, search forward; if negative, search backward.

If the item under point can be worked, work on it and return;
If not, search for the first opportunity before LIMIT is reached;
If the search finds something, work on it and return."
  (unless (+/-:plus-minus-at-point step)
    (let ((point (point)))
      (when (+/-:search forward? limit)
	(+/-:plus-minus-at-point step)))))

(defun +/-:plus (&optional forward? step limit)
  "If FORWARD? is positive, move forward and plus STEP amount on the first
found item before LIMIT is reached;
if FORWARD? is negative, move backward and perform the plus operation."
  (let ((forward? (or forward? +/-:forward?))
	(step (or step +/-:step))
	(limit (or limit (point-at-eol))))
    (+/-:plus-minus forward? step limit)))

(defun +/-:minus (&optional forward? step limit)
  "If FORWARD? is positive, move forward and minus STEP amount on the first
found item before LIMIT is reached;
if FORWARD? is negative, move backward and perform the minus operation."
  (+/-:plus-plus forward? (- step) limit))

(defun +/-:plus-minus-region (rb re step)
  "Perform the plus-minus operation on a region, starting from RB (region beginning)
to RE (region end) with STEP amount of increment."
  (save-excursion
    (goto-char rb)
    (while (<= (point) re)
      (+/-:plus-minus +1 step (min (point-at-eol) re))
      (forward-char))))

(defun +/-:plus-minus-block (bb be step)
  "Perform the plus-minus operation on a block, also known as rectangle,
starting from BB (block beginning) to BE (block end) with STEP amount of increment."
  (save-excursion
    (goto-char bb)
    (let ((alist (extract-rectangle-bounds bb be))
	  (i 0))
      (dolist (elm alist)
	(let ((end (cdr elm)))
	  (+/-:plus-minus +1
			  (if (> step 0)
			      (+ step i)
			    (- step i))
			  end))
	(next-line)
	(setq i (1+ i))))))

;;;###autoload
(defun +/-:forward+ (&optional step)
  "Perform a forward plus operation with optional STEP amount of increment.

STEP defaults to 1."
  (interactive "p")
  (+/-:plus +1 step))

;;;###autoload
(defun +/-:forward- (&optional step)
    "Perform a forward minus operation with optional STEP amount of decrement.

STEP defaults to 1."
  (interactive "p")
  (+/-:forward+ (- step)))

;;;###autoload
(defun +/-:backward+ (&optional step)
  "Perform a backward plus operation with optional STEP amount of increment.

STEP defaults to 1."
  (interactive "p")
  (+/-:plus -1 step (point-at-bol)))

;;;###autoload
(defun +/-:backward- (&optional step)
    "Perform a backward minus operation with optional STEP amount of decrement.

STEP defaults to 1."
  (interactive "p")
  (+/-:backward+ (- step)))

;;;###autoload
(defun +/-:region+ (&optional step)
    "Perform a plus operation on a region with optional STEP amount of decrement.

STEP defaults to 1."
  (interactive "p")
  (when (region-active-p)
    (let (deactivate-mark
	  (rb (region-beginning))
	  (re (region-end)))
      (+/-:plus-minus-region rb re step))))

;;;###autoload
(defun +/-:region- (&optional step)
    "Perform a minus operation on a region with optional STEP amount of decrement.

STEP defaults to 1."
  (interactive "p")
  (+/-:region+ (- step)))

;;;###autoload
(defun +/-:block+ (start end &optional step)
    "Perform a plus operation on a block (also known as rectangle) with
optional STEP amount of decrement.

STEP defaults to 1."
  (interactive "r\np")
  (+/-:plus-minus-block start end step))

;;;###autoload
(defun +/-:block- (start end &optional step)
    "Perform a minus operation on a block (also known as rectangle) with
optional STEP amount of decrement.

STEP defaults to 1."
  (interactive "r\np")
  (+/-:block+ start end (- step)))

(provide 'plus-minus)
;;; plus-minus.el ends here
