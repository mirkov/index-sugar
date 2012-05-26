;; Mirko Vukovic
;; Time-stamp: <2012-05-25 21:29:41 helper-functions.lisp>
;; 
;; Copyright 2011 Mirko Vukovic
;; Distributed under the terms of the GNU General Public License
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :index-sugar)

(defun subst-alpha (string alpha)
  "Substitute occurances of \"ALPHA\" in `string' by
numeric values of `alpha':
 (subst-alpha \"foo-alpha\" 3) results in \"foo-3\"
"
  (let* ((s-alpha (format nil "~D" alpha))
	 (comps (split-sequence #\- string))
	 (res (first comps)))
    (dolist (comp (rest comps))
      (setf res (concatenate 'string res "-"
			     (cond ((string= comp "ALPHA") s-alpha)
				   (t comp)))))
    res))

(define-test subst-alpha
  (assert-equal "foo-3" (subst-alpha "foo-ALPHA" 3)))

(defun subst-alpha-beta (string alpha beta)
  "Substitute occurances of \"alpha\" and \"beta\" in `string' by
numeric values of `alpha' and `beta':
 (subst-alpha-beta \"foo-alpha-beta\" 3 8) results in \"foo-3-8\"
"
  (let* ((s-alpha (format nil "~D" alpha))
	 (s-beta (format nil "~D" beta))
	 (comps (split-sequence #\- string))
	 (res (first comps)))
    (dolist (comp (rest comps))
      (setf res (concatenate 'string res "-"
			     (cond ((string= comp "ALPHA") s-alpha)
				   ((string= comp "BETA") s-beta)
				   (t comp)))))
    res))

(define-test subst-alpha-beta
  (assert-equal "foo-3-8" (subst-alpha-beta "foo-ALPHA-BETA" 3 8))
  (assert-equal "foo-alpha-8" (subst-alpha-beta "foo-alpha-BETA" 3 8)))


;;; following functions are for array indexing

#|(defun index-ref->index-sexp (index-ref-str &optional (base-offset 0))
  "Convert `index-ref-str' to an s-expression that calculates
the absolute index using BASE-OFFSET"
  (let ((index-ref-str-reread
	 (symbolicate index-ref-str) #|(onlisp:reread index-ref-str)|#))
    (if (numberp index-ref-str-reread)
	`(+ ,index-ref-str-reread (- ,base-offset))
	(multiple-value-bind (index offset)
	    (parse-index index-ref-str)
	  `(+ ,index ,@(when offset `(,offset)) (- ,base-offset))))))|#


#|(define-test index-ref->index-sexp
  (assert-equal '(+ I (- 0))
		(index-ref->index-sexp "i"))
  (assert-equal '(+ 1 (- 2))
		(index-ref->index-sexp "1" 2))
  (assert-equal '(+ I (- 0))
		(index-ref->index-sexp "i" 0))
  (assert-equal '(+ I (- 1))
		(index-ref->index-sexp "i" 1))
  (assert-equal '(+ I 1 (- 0))
		(index-ref->index-sexp "i+1" 0))
  (assert-equal '(+ I -1 (- 1))
		(index-ref->index-sexp "i-1" 1))
  (assert-equal '(+ I -1 (- 10))
		(index-ref->index-sexp "i-1" 10))
  (assert-equal '(+ I -10 (- b))
		(index-ref->index-sexp "i-10" 'b))
  (assert-equal '(+ I -10 (- 1))
		(index-ref->index-sexp "i-10" 1))
  (assert-equal '(+ I -1/2 (- -1/2))
		(index-ref->index-sexp "i-1/2" -1/2)
		"fraction offset test"))|#

(defun sub-trans (indexed-var &key offsets (access-fun 'aref))
  "Generate a list of symbol-macrolet translations for the
`indexed-var' using the `offsets' list.  If offsets is nil, the
default offset is used.

If offsets are not provided, use the value in *offset*"
  (destructuring-bind (var &rest indices)
      (mapcar (lambda (arg)
		(symbolicate arg))
	      #|#'onlisp:reread|#
	      (split-sequence
	       #\_ (symbol-name indexed-var)))
    (let* ((offsets
	    (if offsets offsets
		(loop for index in indices
		   collect *offset*)))
	   (index-sexps
	    (mapcar #'(lambda (index-ref index-offset)
			`(- ,(multiple-value-bind
			       (terms operators)
			       (split-infix (symbol-name index-ref))
			       (parse-algebraic-expression
				(mapcar #'read-from-string terms)
				operators))
			    ,index-offset))
		    indices offsets)))
      `(,indexed-var (,access-fun ,var
				,@index-sexps)))))

(define-test sub-trans
  (assert-equal '(x_0 (aref x (- 0 0)))
		(sub-trans 'x_0))
  (assert-equal '(x_0 (aref x (- 0 0)))
		(sub-trans 'x_0 :offsets '(0)))
  (assert-equal '(x_0 (aref x (- 0 1)))
		(sub-trans 'x_0 :offsets '(1)))
  (assert-equal '(x_i (aref x (- i 0)))
		(sub-trans 'x_i :offsets '(0)))
  (assert-equal '(x_i (aref x (- i 1)))
		(sub-trans 'x_i :offsets '(1)))
  (assert-equal '(x_i-1/2 (aref x (- (- i (/ 1 2)) -1/2)))
		(sub-trans 'x_i-1/2 :offsets '(-1/2)))
  (assert-equal '(x_i_j_k (aref x (- i 0) (- j 1) (- k 2)))
		(sub-trans 'x_i_j_k :offsets '(0 1 2)))
  (assert-equal '(x_i_j_k (maref x (- i 0) (- j 1) (- k 2)))
		(sub-trans 'x_i_j_k :offsets '(0 1 2) :access-fun 'maref)))
