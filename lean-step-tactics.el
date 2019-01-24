;;; lean-step-tactics.el --- step tactics, resizing Lean Goal window

;; Copyright (C) 2019 Joseph Corneli
;; Author: Joseph Corneli <holtzermann17@gmail.com>
;; Version: 1.0

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

;;; Documentation:

;; Navigate forwards and backwards applying/unapplying tactic steps
;; using M-n, M-p respectively.  The Lean Goal window will be resized
;; to fit contents.

;; Bug: Emacs doesn't always get the resizing right the first time,
;; but will sort it out if you move backwards and forwards again.

;;; Code:

(require 'cl-macs)

(cl-defun lean-forward-to-end-of-line (&optional (direction 1))
  (interactive)
  (forward-line direction)
  (end-of-line)
  (let ((orig (current-buffer)))
    (pop-to-buffer
     (get-buffer lean-show-goal-buffer-name))
    (fit-window-to-buffer nil nil (+ (count-lines (point-min)
						  (point-max))
				     3))
    (pop-to-buffer orig)))

(defun lean-backward-to-end-of-line ()
  (interactive)
  (lean-forward-to-end-of-line -1))

(define-key lean-mode-map "\M-n" 'lean-forward-to-end-of-line)
(define-key lean-mode-map "\M-p" 'lean-backward-to-end-of-line)

(provide 'lean-step-tactics)
