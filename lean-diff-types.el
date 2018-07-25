
;;; lean-diff-types.el --- A Lean utility copied from company-coq -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2016, 2017  Clément Pit-Claudel
;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
;; URL: https://github.com/cpitclaudel/company-coq
;; Keywords: convenience, languages
;; Package-Requires: ((cl-lib "0.5") (dash "2.12.1") (yasnippet "0.11.0") (company "0.8.12") (company-math "1.1"))
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

(require 'lean-mode)

(defmacro lean-with-current-buffer-maybe (bufname &rest body)
  "taken from `company-coq`
   If BUFNAME is a live buffer, run BODY in it."
  (declare (indent defun)
           (debug t))
  `(-when-let* ((bufname ,bufname)
                (buf (get-buffer bufname)))
     (with-current-buffer buf
       ,@body)))

(defun lean--make-diff-temp-buffer (bufname string prefix)
  "taken from `company-coq`
   Insert STRING into BUFNAME, with optional PREFIX."
  (with-current-buffer (get-buffer-create bufname)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when prefix (insert prefix))
      (insert string)
      (newline))
    (current-buffer)))

(defun lean-diff-strings (s1 s2 prefix diff-buffer-name context)
  "taken from `company-coq`
   Compare S1 and S2, with PREFIX.
Use DIFF-BUFFER-NAME to name newly created buffers.  Display CONTEXT lines
of context around differences."
  (let ((same-window-buffer-names '("*Diff*"))
        (b1 (lean--make-diff-temp-buffer (format "*lean-%s-A*" diff-buffer-name) s1 prefix))
        (b2 (lean--make-diff-temp-buffer (format "*lean-%s-B*" diff-buffer-name) s2 prefix)))
    (set-window-dedicated-p (selected-window) nil)
    (lean-with-current-buffer-maybe diff-buffer-name
      (kill-buffer))
    (unwind-protect
        (diff b1 b2 `(,(format "--unified=%d" context) "--minimal" "--ignore-all-space") 'noasync)
      (kill-buffer b1)
      (kill-buffer b2))
    (lean-with-current-buffer-maybe "*Diff*"
      (diff-refine-hunk)
      (rename-buffer diff-buffer-name)
      (save-excursion
        (goto-char (point-min))
        (let ((inhibit-read-only t))
          (when (re-search-forward "^@@" nil t)
            (delete-region (point-min) (match-beginning 0)))
          (while (re-search-forward " *\n *\n\\( *\n\\)+" nil t)
            ;; Remove spurious spacing added to prevent diff from mixing terms
            (replace-match "\n" t t)))))))

(defun lean-not-is-expected (x)
  (not (equal "but is expected to have type" x)))
(defun lean-not-is-actual (x)
  (not (equal "has type" x)))

(defun lean-diff-types ()
  (interactive)
  (let* ((errs (with-current-buffer (get-buffer lean-next-error-buffer-name)
                  (buffer-substring-no-properties (point-min) (point-max))))
         (lns (split-string errs "\n"))
         (msg (seq-take-while 'lean-not-is-actual lns))
         (types (seq-drop (seq-drop-while 'lean-not-is-actual lns) 1 ))
         (expected (seq-take-while (lambda (x) (not (string-empty-p x)))
                                   (seq-drop (seq-drop-while 'lean-not-is-expected types) 1)))
         (actual (seq-take-while 'lean-not-is-expected types)))
    (unless (null expected)
      (save-selected-window
        (with-current-buffer (get-buffer-create "*lean-diff*")
          (pop-to-buffer
           ;; (switch-to-buffer-other-window
           (current-buffer) '())
          (lean-diff-strings (mapconcat 'identity actual "\n")
                             (mapconcat 'identity expected "\n")
                             "type: " "*lean-diff*" 5)
          (with-current-buffer (get-buffer-create "*lean-diff*")
            (let ((buffer-read-only nil))
              (save-excursion
                (insert (mapconcat 'identity msg "\n" ))
                (insert "\n")))))))))

(defcustom lean-keybinding-diff-types (kbd "C-c C-t")
  "Lean Keybinding for lean-diff-type"
  :group 'lean-keybinding :type 'key-sequence)
(local-set-key lean-keybinding-diff-types                 #'lean-diff-types)
(provide 'lean-diff-types)
