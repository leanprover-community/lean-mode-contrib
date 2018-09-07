;;; lean-filladapt.el --- improve filling long comments in lean mode

;; Copyright (C) 2018 Joseph Corneli
;; Author: Joseph Corneli <holtzermann17@gmail.com>
;; Package-Requires: ((lean-mode "20180906.1645") (filladapt "2.12.2") (diminish "0.44")
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

;; Use filladapt to improve the way Lean fills long comment strings.
;; It now works intelligently with both kinds of comments.

;; In particular, you will see the right behaviour for both of these:

;;   -- This is a long inline comment This is a long inline comment This is a long inline comment This is a long inline comment This is a long inline comment

;;   /- This is a long block comment This is a long block comment This is a long block comment This is a long block comment This is a long block comment -/

;; (That's an improvement over the out-of-the-box behaviour of
;; lean-mode, which gets the second one wrong.)

;;; Code:

(require 'filladapt)
(require 'diminish)

(add-hook 'lean-mode-hook #'filladapt-mode)
(diminish 'filladapt-mode)

(defun set-lean-filladapt-token-table ()  
  (set (make-local-variable 'filladapt-token-table)
       '(("^" beginning-of-line)
	 (">+" citation->)
	 ("\\(\\w\\|[0-9]\\)[^'`\"< 	
]*>[ 	]*" supercite-citation)
	 (";+" lisp-comment)
	 ("--" lisp-comment)
	 ("#+" sh-comment)
	 ("%+" postscript-comment)
	 ("" c-comment)
	 ("///*" c++-comment)
	 ("@c[ 	]" texinfo-comment)
	 ("@comment[ 	]" texinfo-comment)
	 ("\\\\item[ 	]" bullet)
	 ("[0-9]+\\.[ 	]" bullet)
	 ("[0-9]+\\(\\.[0-9]+\\)+[ 	]" bullet)
	 ("[A-Za-z]\\.[ 	]" bullet)
	 ("(?[0-9]+)[ 	]" bullet)
	 ("(?[A-Za-z])[ 	]" bullet)
	 ("[0-9]+[A-Za-z]\\.[ 	]" bullet)
	 ("(?[0-9]+[A-Za-z])[ 	]" bullet)
	 ("[-~*+]+[ 	]" bullet)
	 ("o[ 	]" bullet)
	 ("[ 	]+" space)
	 ("$" end-of-line))))

(add-hook 'lean-mode-hook #'set-lean-filladapt-token-table)

