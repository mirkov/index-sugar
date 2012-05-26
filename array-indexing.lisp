;; Mirko Vukovic
;; Time-stamp: <2012-05-25 21:20:32 array-indexing-notation2.lisp>
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


(defmacro assert-symbol-macrolet-expansion (expected &body body)
  `(assert-expands
    '(symbol-macrolet (,@expected)
      ,@body)
    (with-_-indexing
      ,@body)))

(defmacro with-_-indexing (&body body)
  "WITH-_-INDEXING uses SYMBOL-MACROLET to lexically establish
expansions for each symbol that contains an underscore in its name.

Such symbols are expanded into AREF forms.  In the simplest case, the
symbol x_i is expanded into (aref x i).

Symbols with multiple underscores are expanded into arefs across
multiple array dimensions.  For example, x_i_j_k expands into (aref x
i j k).

Special variable *OFFSET* (default value is zero) can be used to
provide an offset to the array indes.  For example

\(let ((*offset* 1))
   (with-_-indexing x_i))

leads to the expansion (aref x (- i 1))

The spacial variable *AREF-FUN* (default 'aref) can be used to use a
an accessor function other than AREF.  Thus when using the GRIDS
package, one can set *AREF-FUN* to 'gref."
  (let ((syms (remove-if-not
	       #'_symb-p
	       (unique-syms (alexandria:flatten body))))
	translations)
    (dolist (sym syms)
      (push (sub-trans sym)
	    translations))
    `(symbol-macrolet ,(nreverse translations)
       ,@body)))



(define-test with-_-indexing
  (assert-symbol-macrolet-expansion
      ((n_j (aref n (- j 0)))
       (n_i (aref n (- i 0)))
       (n_i+1 (aref n (- (+ i 1) 0))))
    (* n_j (+ n_i n_i+1)))
  (assert-symbol-macrolet-expansion
      ((n_j (aref n (- j 0)))
       (n_i (aref n (- i 0)))
       (n_i+1 (aref n (- (+ i 1) 0))))
    (* n_j (+ n_i n_i+1)))
  (assert-symbol-macrolet-expansion
      ((n_0 (aref n (- 0 0)))
       (n_1 (aref n (- 1 0)))
       (n_12 (aref n (- 12 0))))
    (* n_0 (+ n_1 n_12)))
  (assert-symbol-macrolet-expansion
      ((n_j (aref n (- j 0)))
       (n_i (aref n (- i 0)))
       (n_i+1 (aref n (- (+ i 1) 0))))
    (* n_j (+ n_i n_i+1)))
  (assert-symbol-macrolet-expansion
      ((n_i_j_k
	(aref n (- i 0) (- j 0) (- k 0)))
       (m_i_j_k
	(aref m (- i 0) (- j 0) (- k 0))))
    (+ 1d0 n_i_j_k m_i_j_k))
  (let ((*offset* 1)
	(*aref-fun* 'gref))
    (assert-symbol-macrolet-expansion
	((n_i_j_k
	  (gref n (- i 1) (- j 1) (- k 1)))
	 (m_i_j_k
	  (gref m (- i 1) (- j 1) (- k 1))))
      (+ 1d0 n_i_j_k m_i_j_k))))

(defmacro with-_-indexing1 ((&key offsets (access-fun 'aref))
			    &body body)
  "WITH-_-INDEXING1 uses SYMBOL-MACROLET to lexically establish
expansions for each symbol that contains an underscore in its name.

See documentation of WITH-_-INDEXING for details on the macro usage.

The macro is similar to WITH-_-INDEXING.  It enhances it by allowing
the user to specify offsets for each dimension, and to specify the
referencing function.

OFFSETS -- an n-element list
ACCESS-FUN -- a symbol

All symbols must have the same number, N, of underscores (and thus
indices).  Each element of OFFSETS applies to the corresponding index

ACCESS-FUN can be used to specify an access fun other than AREF"
  (let ((syms (remove-if-not
	       #'_symb-p
	       (unique-syms (alexandria:flatten body))))
	translations)
    (dolist (sym syms)
      (push (sub-trans sym
		       :offsets offsets
		       :access-fun access-fun)
	    translations))
    `(symbol-macrolet ,(nreverse translations)
       ,@body)))


(define-test with-_-indexing1
  (assert-expands
   '(symbol-macrolet ((n_j (gref n (- j 1)))
		     (n_i (gref n (- i 1)))
		     (n_i+1 (gref n (- (+ i 1) 1))))
         (* n_j (+ n_i n_i+1)))
   (with-_-indexing1 (:offsets (1) :access-fun gref)
     (* n_j (+ n_i n_i+1)))))


(define-test matrix-multiplication
  (let ((a (make-array '(3 2) :initial-contents
		       '((9 3)
			 (2 5)
			 (3 8))))
	(b (make-array '(2 2) :initial-contents
		       '((1 4)
			 (6 7))))
	(c (make-array '(3 2))))
    (with-_-indexing1 (:offsets (1 1))
      (loop for i from 1 to 3
	   do (loop for j from 1 to 2
		 do (setf c_i_j
			  (loop for k from 1 to 2
			       summing (+ (* a_i_k b_k_j)))))))
	(assert-numerical-equal
	 (make-array '(3 2) :initial-contents
		     '((27 57)
		       (32 43)
		       (51 68)))
	 c)))
	   

#|


(defmacro with-_-indexing2 (index-refs &body body)
  "Same as `with-_-index-refs', but allowing for different
substitutions for different variables.  `index-refs' is a list whose
car is the anchor string and whose rest are the substitution rules.

This is useful if different variables use different inexing schemes."
  (let ((syms (unique-syms (alexandria:flatten body)))
	translations)
    (dolist (subst-def index-refs)
      (let ((anchor-name (symbol-name (first subst-def)))
	    (index-substitutions (rest subst-def)))
	(let ((a-syms (remove-if-not 
		       #'(lambda (symb)
			   (= (length anchor-name)
			      (mismatch anchor-name
					(symbol-name symb))))
		       syms)))
	  (dolist (a-sym a-syms)
	    (push (sub-trans a-sym index-substitutions)
		  translations)))))
    `(symbol-macrolet ,(nreverse translations)
       ,@body)))

(defmacro with-_-indexing3 (index-refs &body body)
  "Combination of `with-_-index-refs1' and `with-_-index-refs2':
Multiple variables have multiple indexing schemes, and each can have
optionally their own referencing function (default is aref).
Index-refs format is: ((a (a-list) [ref-func]) (b (a-list)
[ref-func])) ..."
  (let ((syms (unique-syms (alexandria:flatten body)))
	translations)
    (dolist (subst-def index-refs)
      (destructuring-bind (anchor index-substitutions &optional (ref-func 'aref))
	  subst-def
	(let ((a-syms (remove-if-not 
		       #'(lambda (symb)
			   (let ((anchor-name (symbol-name anchor)))
			     (= (length anchor-name)
				(mismatch anchor-name
					  (symbol-name symb)))))
		       syms)))
	  (dolist (a-sym a-syms)
	    (push (sub-trans a-sym index-substitutions :ref-func ref-func)
		  translations)))))
    `(symbol-macrolet ,(nreverse translations)
       ,@body)))

|#







(define-test array-access
    (let ((arr (make-array '(2 2) :initial-element 0d0)))
      (with-_-indexing ((arr 0 0))
	(setf arr_0_0 1d0
	      arr_1_1 2d0)
	(assert-numerical-equal
       #2A((1d0 0d0) (0d0 2d0)) arr))))


