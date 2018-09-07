
;;; lean-quick-peek.el --- integrate lean with quick-peek

;; Copyright (C) 2018 Joseph Corneli
;; Author: Joseph Corneli <holtzermann17@gmail.com>
;; Package-Requires: ((lean-mode "20180906.1645") (quick-peek "20180525.1411")
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

;; Use Quick Peek and Flycheck to view *fontified* results of Lean operations inline

;;; Code:

(require 'lean-mode)
(require 'quick-peek)

(with-eval-after-load 'flycheck
  (flycheck-inline-mode))

(setq flycheck-inline-display-function
      (lambda (msg pos)
        (let* ((ov (quick-peek-overlay-ensure-at pos))
               (contents (quick-peek-overlay-contents ov)))
          (setf (quick-peek-overlay-contents ov)
                (concat contents (when contents "\n")
			(save-excursion
			  (set-buffer (get-buffer-create "*Flycheck-Inline*"))
			  (erase-buffer)
			  (flet ((lean-mode-setup () nil))
			    (let (lean-hooks-alist)
			    (lean-mode)))
			  (insert msg)
			  (font-lock-fontify-region (point-min) (point-max) nil)
			  (buffer-string))))
          (quick-peek-update ov)))
      flycheck-inline-clear-function #'quick-peek-hide)

(provide 'lean-quick-peek)
