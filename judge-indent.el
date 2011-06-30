;;; judge-indent.el --- judge indent and tab widths

;;; Copyright (C) 2011 yascentur

;; Author:   yascentur <screenname at gmail dot com>
;; Keywords: indent tab
;; Version:  1.1.0b

;; This program is free software; you can redistribute it and/or modify
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

;; The judge-indent.el judges indent and tab widths
;; into following 8 (strictly 7) patterns.
;;
;;   \  indent
;;    \  2 4 8
;; tab \------
;;   4 | o c -
;;   8 | o o c <- It can not distinguish between (8, 8) and (4, 4)
;; nil | o o o

;;; Usage:

;; Add following 3 lines into your emacs config file
;;   (require 'judge-indent)
;;   (global-judge-indent-mode t)
;;   (setq judge-indent-major-modes '(c-mode python-mode sh-mode))

;;; Customization:

;; Set default indent width (2, 4 or 8)
;;   (setq judge-indent-default-indent-width 4)
;; The default value is `c-basic-offset' or 4
;;
;; Set default tab width (4 or 8)
;;   (setq judge-indent-default-tab-width 8)
;; The default value is `tab-width' or 8
;;
;; Set flag of preferring tab or not when indent is not so deep
;;   (setq judge-indent-prefer-tabs-mode nil)
;; The default value is `indent-tabs-mode' or nil
;;
;; Set relative tolerance [%] for judging indent and tab widths
;;   (setq judge-indent-relative-tolerance 5)
;; The default value is 5 %
;;
;; Set search limit for large size files
;;   (setq judge-indent-search-limit 30000)
;; The default value is 30000 chars (equal to ca. 1000 lines)

;;; Functions:

;; * judge-indent-mode
;; * judge-indent-buffer
;; * judge-indent-region
;; * judge-indent-set-indent-tab-widths
;; * judge-indent-set-indent-width{2, 4, 8}-disable-tab
;; * judge-indent-set-indent-width{2, 4, 8}-tab-width{2, 8}
;; * judge-indent-set-indent-width
;; * judge-indent-set-indent-width{2, 4, 8}
;; * judge-indent-set-tab-width
;; * judge-indent-disable-tab
;; * judge-indent-set-tab-width{4, 8}
;; * judge-indent-message-indent-counts-buffer
;; * judge-indent-message-indent-counts-region

;;; Code:

(eval-when-compile (require 'cl))

(defgroup judge-indent nil
  "Judge indent"
  :group  'convenience
  :prefix "judge-indent-")

(defcustom judge-indent-major-modes '()
  "Major modes of applying judge-indent-mode"
  :type  '(list symbol)
  :group 'judge-indent)

(defcustom judge-indent-default-indent-width
  (if (default-boundp 'c-basic-offset)
      (default-value  'c-basic-offset) 4)
  "Default indent width (2, 4 or 8)"
  :type  'number
  :group 'judge-indent)

(defcustom judge-indent-default-tab-width
  (if (default-boundp 'tab-width)
      (default-value  'tab-width) 8)
  "Default tab width (4 or 8)"
  :type  'number
  :group 'judge-indent)

(defcustom judge-indent-prefer-tabs-mode
  (if (default-boundp 'indent-tabs-mode)
      (default-value  'indent-tabs-mode) nil)
  "Prefer tab or not when indent is not so deep"
  :type  'boolean
  :group 'judge-indent)

(defcustom judge-indent-relative-tolerance 5
  "Relative tolerance [%] for judging indent and tab widths"
  :type  'number
  :group 'judge-indent)

(defcustom judge-indent-search-limit 30000
  "Search limit for large size files (30000 chars equal to ca. 1000 lines)"
  :type  'number
  :group 'judge-indent)

(defvar judge-indent-indent-width judge-indent-default-indent-width
  "Indent width")
(defvar judge-indent-tab-width judge-indent-default-tab-width
  "Tab width")
(defvar judge-indent-count-1tab 0
  "Count of 1-tab indents")
(defvar judge-indent-count-2space 0
  "Count of 2-space indents")
(defvar judge-indent-count-4space 0
  "Count of 4-space indents")
(defvar judge-indent-count-8space 0
  "Count of 8-space indents")
(defvar judge-indent-minor-mode-lighter "JI"
  "Minor mode lighter")

(defun judge-indent-set-minor-mode-lighter (indent tab)
  "Set minor mode lighter"
  (setq judge-indent-minor-mode-lighter
        (concat " JI:" (number-to-string indent)
                "[" (if (= tab 0) "-" (number-to-string tab)) "]"))
  (setcar (cdr (assq 'judge-indent-mode minor-mode-alist))
          'judge-indent-minor-mode-lighter))

;; set indent width

(defun judge-indent-set-specific-indent-width (symbol indent)
  "Set specific mode indent width"
  (when (default-boundp 'symbol) (setq symbol indent)))

(defun judge-indent-set-indent-width-without-message (indent)
  "Set indent width without message"
  (setq judge-indent-indent-width indent)
  (judge-indent-set-specific-indent-width 'c-basic-offset            indent)
  (judge-indent-set-specific-indent-width 'indent-level              indent)
  (judge-indent-set-specific-indent-width 'standard-indent           indent)
  (judge-indent-set-specific-indent-width 'c-indent-level            indent)
  (judge-indent-set-specific-indent-width 'python-indent             indent)
  (judge-indent-set-specific-indent-width 'perl-indent-level         indent)
  (judge-indent-set-specific-indent-width 'cperl-indent-level        indent)
  (judge-indent-set-specific-indent-width 'ruby-indent-level         indent)
  (judge-indent-set-specific-indent-width 'html-basic-offset         indent)
  (judge-indent-set-specific-indent-width 'sgml-basic-offset         indent)
  (judge-indent-set-specific-indent-width 'html-helper-basic-offset  indent)
  (judge-indent-set-specific-indent-width 'yahtml-environment-indent indent)
  (judge-indent-set-specific-indent-width 'nxml-child-indent         indent)
  (judge-indent-set-specific-indent-width 'css-indent-level          indent)
  (judge-indent-set-specific-indent-width 'cssm-indent-level         indent)
  (judge-indent-set-specific-indent-width 'javascript-indent-level   indent)
  (judge-indent-set-specific-indent-width 'js-indent-level           indent)
  (judge-indent-set-specific-indent-width 'js2-basic-offset          indent)
  (judge-indent-set-specific-indent-width 'sh-basic-offset           indent))

(defun judge-indent-set-indent-width (indent)
  "Set indent width"
  (interactive "nIndent width: ")
  (message (concat "Set indent width to " (number-to-string indent) "..."))
  (judge-indent-set-minor-mode-lighter indent judge-indent-tab-width)
  (judge-indent-set-indent-width-without-message indent))

(defun judge-indent-set-indent-width2 ()
  "Set indent width to 2"
  (interactive)
  (judge-indent-set-indent-width 2))

(defun judge-indent-set-indent-width4 ()
  "Set indent width to 4"
  (interactive)
  (judge-indent-set-indent-width 4))

(defun judge-indent-set-indent-width8 ()
  "Set indent width to 8"
  (interactive)
  (judge-indent-set-indent-width 8))

;; set tab width

(defun judge-indent-set-tab-width-without-message (tab)
  "Set tab width without message"
  (setq judge-indent-tab-width tab)
  (if (= tab 0)
      (setq indent-tabs-mode nil)
    (progn
      (setq indent-tabs-mode t)
      (setq tab-width tab)
      (setq tab-stop-list
            '((* tab  1) (* tab  2) (* tab  3) (* tab  4) (* tab  5)
              (* tab  6) (* tab  7) (* tab  8) (* tab  9) (* tab 10)
              (* tab 11) (* tab 12) (* tab 13) (* tab 14) (* tab 15))))))

(defun judge-indent-set-tab-width (tab)
  "Set tab width"
  (interactive "nTab width: ")
  (message (concat (if (= tab 0) "Disable tab"
                     (concat "Set tab width to " (number-to-string tab)))
                   "..."))
  (judge-indent-set-minor-mode-lighter judge-indent-indent-width tab)
  (judge-indent-set-tab-width-without-message tab))

(defun judge-indent-disable-tab ()
  "Disable tab"
  (interactive)
  (judge-indent-set-tab-width 0))

(defun judge-indent-set-tab-width4 ()
  "Set tab width to 4"
  (interactive)
  (judge-indent-set-tab-width 4))

(defun judge-indent-set-tab-width8 ()
  "Set tab width to 8"
  (interactive)
  (judge-indent-set-tab-width 8))

;; set indent and tab widths

(defun judge-indent-set-indent-tab-widths (indent tab)
  "Set indent and tab widths"
  (interactive "nIndent Width: \nnTab width: ")
  (message (concat "Set indent width to " (number-to-string indent)
                   (if (= tab 0) " and disable tab"
                     (concat " and tab width to " (number-to-string tab)))
                   "..."))
  (judge-indent-set-minor-mode-lighter indent tab)
  (judge-indent-set-indent-width-without-message indent)
  (judge-indent-set-tab-width-without-message    tab))

(defun judge-indent-set-indent-width2-disable-tab ()
  "Set indent width to 2 and disable tab"
  (interactive)
  (judge-indent-set-indent-tab-widths 2 0))

(defun judge-indent-set-indent-width4-disable-tab ()
  "Set indent width to 4 and disable tab"
  (interactive)
  (judge-indent-set-indent-tab-widths 4 0))

(defun judge-indent-set-indent-width8-disable-tab ()
  "Set indent width to 8 and disable tab"
  (interactive)
  (judge-indent-set-indent-tab-widths 8 0))

(defun judge-indent-set-indent-width2-tab-width4 ()
  "Set indent width to 2 and tab width to 4"
  (interactive)
  (judge-indent-set-indent-tab-widths 2 4))

(defun judge-indent-set-indent-width4-tab-width4 ()
  "Set indent width to 4 and tab width to 4"
  (interactive)
  (judge-indent-set-indent-tab-widths 4 4))

(defun judge-indent-set-indent-width2-tab-width8 ()
  "Set indent width to 2 and tab width to 8"
  (interactive)
  (judge-indent-set-indent-tab-widths 2 8))

(defun judge-indent-set-indent-width4-tab-width8 ()
  "Set indent width to 4 and tab width to 8"
  (interactive)
  (judge-indent-set-indent-tab-widths 4 8))

(defun judge-indent-set-indent-width8-tab-width8 ()
  "Set indent width to 8 and tab width to 8"
  (interactive)
  (judge-indent-set-indent-tab-widths 8 8))

;; judge indent and tab widths

(defun judge-indent-judge-from-indent-counts ()
  "Judge indent from indent counts"
  (let ((tolerance 0))
    (setq tolerance
          (/ (* (+ judge-indent-count-1tab
                   judge-indent-count-2space
                   judge-indent-count-4space
                   judge-indent-count-8space)
                judge-indent-relative-tolerance) 100))
    (if (and (= judge-indent-count-1tab   0)
             (= judge-indent-count-2space 0)
             (= judge-indent-count-4space 0)
             (= judge-indent-count-8space 0))
        (judge-indent-set-indent-tab-widths
         judge-indent-default-indent-width
         (if judge-indent-prefer-tabs-mode
             judge-indent-default-tab-width 0))
      (if (<= judge-indent-count-1tab tolerance)
          (if (and (<= judge-indent-count-4space tolerance)
                   (<= judge-indent-count-2space tolerance))
              ;; indent width = 8
              (judge-indent-set-indent-tab-widths 8 0)
            (if (<= judge-indent-count-2space tolerance)
                ;; indent width = 4
                (judge-indent-set-indent-tab-widths
                 4
                 (if (and (= judge-indent-count-8space 0)
                          judge-indent-prefer-tabs-mode)
                     8 0))
              ;; indent width = 2
              (judge-indent-set-indent-tab-widths
               2
               (if (and (= judge-indent-count-4space 0)
                        (= judge-indent-count-8space 0)
                        judge-indent-prefer-tabs-mode)
                   judge-indent-default-tab-width
                 (if (and (= judge-indent-count-8space 0)
                          judge-indent-prefer-tabs-mode)
                     8 0)))))
        (if (and (<= judge-indent-count-2space tolerance)
                 (<= judge-indent-count-4space tolerance)
                 (<= judge-indent-count-8space tolerance))
            ;; indent width = tab width
            (judge-indent-set-indent-tab-widths
             judge-indent-default-tab-width
             judge-indent-default-tab-width)
          (if (and (<= judge-indent-count-4space tolerance)
                   (<= judge-indent-count-8space tolerance))
              ;; indent width = 2 & tab width = 4
              (judge-indent-set-indent-tab-widths 2 4)
            (if (<= judge-indent-count-2space tolerance)
                ;; indent width = 4 & tab width = 8
                (judge-indent-set-indent-tab-widths 4 8)
              ;; indent width = 2 & tab width = 8
              (judge-indent-set-indent-tab-widths 2 8))))))))

(defun judge-indent-count-indents (pos1 pos2)
  "Count indents"
  (setq judge-indent-count-1tab   0)
  (setq judge-indent-count-2space 0)
  (setq judge-indent-count-4space 0)
  (setq judge-indent-count-8space 0)
  (save-excursion
    (goto-char pos1)
    (while (and (search-forward "\n\t" judge-indent-search-limit t)
                (<= (point) pos2))
      (unless (or (char-equal (char-after) ?\ )
                  (char-equal (char-after) ?\t))
        (incf judge-indent-count-1tab)))
    (goto-char pos1)
    (while (and (search-forward "\r\t" judge-indent-search-limit t)
                (<= (point) pos2))
      (unless (or (char-equal (char-after) ?\ )
                  (char-equal (char-after) ?\t))
        (incf judge-indent-count-1tab)))
    (goto-char pos1)
    (while (and (search-forward "\n  " judge-indent-search-limit t)
                (<= (point) pos2))
      (unless (or (char-equal (char-after) ?\ )
                  (char-equal (char-after) ?\t))
        (incf judge-indent-count-2space)))
    (goto-char pos1)
    (while (and (search-forward "\r  " judge-indent-search-limit t)
                (<= (point) pos2))
      (unless (or (char-equal (char-after) ?\ )
                  (char-equal (char-after) ?\t))
        (incf judge-indent-count-2space)))
    (goto-char pos1)
    (while (and (search-forward "\n    " judge-indent-search-limit t)
                (<= (point) pos2))
      (unless (or (char-equal (char-after) ?\ )
                  (char-equal (char-after) ?\t))
        (incf judge-indent-count-4space)))
    (goto-char pos1)
    (while (and (search-forward "\r    " judge-indent-search-limit t)
                (<= (point) pos2))
      (unless (or (char-equal (char-after) ?\ )
                  (char-equal (char-after) ?\t))
        (incf judge-indent-count-4space)))
    (goto-char pos1)
    (while (and (search-forward "\n        " judge-indent-search-limit t)
                (<= (point) pos2))
      (unless (or (char-equal (char-after) ?\ )
                  (char-equal (char-after) ?\t))
        (incf judge-indent-count-8space)))
    (goto-char pos1)
    (while (and (search-forward "\r        " judge-indent-search-limit t)
                (<= (point) pos2))
      (unless (or (char-equal (char-after) ?\ )
                  (char-equal (char-after) ?\t))
        (incf judge-indent-count-8space)))))

(defun judge-indent-buffer ()
  "Judge indent and tab widths from buffer"
  (interactive)
  (judge-indent-count-indents (point-min) (point-max))
  (judge-indent-judge-from-indent-counts))

(defun judge-indent-region ()
  "Judge indent and tab widths from region"
  (interactive)
  (judge-indent-count-indents (region-beginning) (region-end))
  (judge-indent-judge-from-indent-counts))

(defun judge-indent-message-indent-counts-buffer ()
  "Message indent counts of buffer"
  (interactive)
  (judge-indent-count-indents (point-min) (point-max))
  (message
   (concat "One-tab: "       (number-to-string judge-indent-count-1tab)
           "; two-space: "   (number-to-string judge-indent-count-2space)
           "; four-space: "  (number-to-string judge-indent-count-4space)
           "; eight-space: " (number-to-string judge-indent-count-8space)
           ";")))

(defun judge-indent-message-indent-counts-region ()
  "Message indent counts of region"
  (interactive)
  (judge-indent-count-indents (region-beginning) (region-end))
  (message
   (concat "One-tab: "       (number-to-string judge-indent-count-1tab)
           "; two-space: "   (number-to-string judge-indent-count-2space)
           "; four-space: "  (number-to-string judge-indent-count-4space)
           "; eight-space: " (number-to-string judge-indent-count-8space)
           ";")))

;; minor mode

(define-minor-mode judge-indent-mode
  "Judge indent mode"
  :lighter " JI"
  :group   'judge-indent
  (if judge-indent-mode
      (progn
        (setq local-enable-local-variables nil)
        (make-local-variable 'judge-indent-minor-mode-lighter)
        (judge-indent-buffer))
    (setq local-enable-local-variables t)
    (kill-local-variable 'judge-indent-minor-mode-lighter)))

(define-global-minor-mode global-judge-indent-mode
  judge-indent-mode
  (lambda ()
    (when (memq major-mode judge-indent-major-modes)
      (judge-indent-mode t))))

(provide 'judge-indent)

;;; judge-indent.el ends here
