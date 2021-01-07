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

;; This package implements plus-minus operations on binary, octal, hexical,
;; decimal digits as well as datetime.

;; It is designed with extensibility in mind and new patterns can be
;; later added.

;; It works like C-a/C-x in Vim, i.e. searches for number up to end of line
;; and then increments or decrements based on context

;; Install:

;; (use-package plus-minus
;;   :ensure t
;;   :bind (("C-c C-a" . +/-:plus-forward)
;;	    ("C-c C-x" . +/-:minus-forward)
;;	    ("C-c M-a" . +/-:plus-backward)
;;	    ("C-c M-x" . +/-:minus-backward)))

;; It supports forward and backward searches and  allows for increments
;; other than the default one.
;; For example, with the above key binding:
;; `C-u 5 C-c C-a’ will let you increment by 5

;;; Code:

(require 'rx)

(defgroup plus-minus ()
  "plus/minus things"
  :group 'lisp
  :prefix "+/-")

(defcustom +/-:step +1
  "Define increment value"
  :type 'number)

(defcustom +/-:forward? +1
  "Define default search direction"
  :type 'number)

(defvar +/-:match-handle-alist
  '((+/-:match-binary  . +/-:handle-binary)
    (+/-:match-octal   . +/-:handle-octal)
    (+/-:match-hexical . +/-:handle-hexical)
    (+/-:match-decimal . +/-:handle-decimal)))

(defvar +/-:match-decimal
  (rx (and (zero-or-one (or ?- ?+))
	   (one-or-more digit))))

(defvar +/-:match-binary
  (rx (and ?0 (= 1 (in "bB"))
	   (one-or-more (in "01")))))

(defvar +/-:match-octal
  (rx (and ?0 (one-or-more (in "0-7")))))

(defvar +/-:match-hexical
  (rx (and ?0 (= 1 (in "xX")) (one-or-more xdigit))))

(defun +/-:format-binary (number)
  (let ((res (when (= number 0) "0")))
    (while (not (= number 0))
      (setq res (concat (number-to-string (% number 2)) res))
      (setq number (ash number -1)))
    (format "0b%s" res)))

(defun +/-:handle-binary (step)
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

(defun +/-:handle-hexical (step)
  (when (and (char-after)
	     (or (and (>= (char-after) ?0)
		      (<= (char-after) ?9))
		 (and (>= (char-after) ?A)
		      (<= (char-after) ?F))
		 (and (>= (char-after) ?a)
		      (<= (char-after) ?f))))
    (let ((point (point)))
      (skip-chars-backward "xX0-9A-Fa-f")
      (if (looking-at +/-:match-hexical)
	  (let ((result (+ step (string-to-number (substring(match-string 0) 2) 16))))
	    (if (< result 0) (setq result 0))
	    (replace-match (format "0x%X" result ))
	    (backward-char)
	    t)
	(goto-char point)
	nil))))

(defun +/-:handle-decimal (step)
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
  "Find things matching patterns before returning matched pattern and range"
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
  (unless (+/-:plus-minus-at-point step)
    (let ((point (point)))
      (when (+/-:search forward? limit)
	(+/-:plus-minus-at-point step)))))

(defun +/-:plus (&optional forward? step limit)
  (let ((forward? (or forward? +/-:forward?))
	(step (or step +/-:step))
	(limit (or limit (point-at-eol))))
    (+/-:plus-minus forward? step limit)))

(defun +/-:minus (&optional forward? step limit)
  (let ((forward? (or forward? +/-:forward?))
	(step (or step +/-:step))
	(limit (or limit (point-at-eol))))
    (+/-:plus-minus forward? (- step) limit)))

(defun +/-:plus-minus-region (rb re step)
  (save-excursion
    (goto-char rb)
    (while (<= (point) re)
      (+/-:plus-minus +1 step (min (point-at-eol) re))
      (forward-char))))

;;;###autoload
(defun +/-:forward+ (&optional step)
  (interactive "p")
  (+/-:plus +1 step))

;;;###autoload
(defun +/-:forward- (&optional step)
  (interactive "p")
  (+/-:forward+ (- step)))

;;;###autoload
(defun +/-:backward+ (&optional step)
  (interactive "p")
  (+/-:plus -1 step (point-at-bol)))

;;;###autoload
(defun +/-:backward- (&optional step)
  (interactive "p")
  (+/-:backward+ (- step)))

;;;###autoload
(defun +/-:region+ (&optional step)
  (interactive "p")
  (when (region-active-p)
    (let (deactivate-mark
	  (rb (region-beginning))
	  (re (region-end)))
      (+/-:plus-minus-region rb re step))))

;;;###autoload
(defun +/-:region- (&optional step)
  (interactive "p")
  (+/-:region+ (- step)))

;;;###autoload
(defun +/-:block+ (&optional step)
  (interactive "p")
  (when (region-active-p)
    (print "region active")))

;;;###autoload
(defun +/-:block- (&optional step)
  (interactive "p")
  (+/-:block+ (- step)))


(provide 'plus-minus)
;;; plus-minus.el ends here
