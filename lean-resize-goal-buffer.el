;;; lean-resize-goal-buffer.el --- fit the Lean Goal window to contents

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

;; With this package loaded, the Lean Goal window will be resized to
;; fit contents.

;; Bug: Emacs doesn't always get the resizing right the first time,
;; but will sort it out if you move backwards and forwards again.

;;; Code:

(defun lean-resize-goal-buffer ()
  (let ((goal-buffer (get-buffer lean-show-goal-buffer-name)))
    (when goal-buffer
      (let ((orig (current-buffer)))
	(pop-to-buffer
	 goal-buffer)
	(fit-window-to-buffer nil nil (+ (count-lines (point-min)
						      (point-max))
					 1))
	(pop-to-buffer orig)))))

(advice-add 'lean-show-goal--handler :after #'lean-resize-goal-buffer)

(provide 'lean-resize-goal-buffer)
