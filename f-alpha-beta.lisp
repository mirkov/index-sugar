;; Mirko Vukovic
;; Time-stamp: <2012-05-27 13:47:15 f-alpha-beta.lisp>
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun build-binding-list-1 (alpha-values fun &rest lambda-list)
    "Build a binding list for fun and lambda-list args
FUN - symbol
LAMBDA-LIST - symbols also
ALPHA-VALUES - index list"
    (let ((fun-name (symbol-name fun)))
      (loop for alpha in alpha-values
	 collect
	 (let ((fun-alpha
		(symbolicate fun-name "-"
			     (format nil "~a" alpha)))
	       (args-alpha
		(loop for arg in lambda-list
		   collect
		   (intern (subst-alpha (symbol-name arg) alpha)))))
	   `(,fun-alpha (,fun ,@args-alpha)))))))

(define-test build-binding-list-1
  (assert-equal '((f-1 (f x-1 y))
		  (f-2 (f x-2 y))
		  (f-3 (f x-3 y)))
		(build-binding-list-1 '(1 2 3) 'f 'x-alpha 'y)))



#|(defmacro with-f-alpha ((fun &rest lambda-list) alpha-values &body body)
  `(let (,@(apply #'build-binding-list-1 alpha-values fun lambda-list))
     ,@body))


(define-test with-f-alpha
  (assert-expands
   '(let ((f-1 (f x-1 y))
	  (f-2 (f x-2 y))
	  (f-3 (f x-3 y)))
     t)
   (with-f-alpha (f x-alpha y) (1 2 3) t)))|#

(defmacro with-f-alpha ((alpha-values &rest fun-defs)
			  &body body)
  `(let (,@(loop for (fun . lambda-list) in fun-defs
	       append (apply #'build-binding-list-1 alpha-values
			      fun lambda-list)))
     ,@body))

(defmacro with-f-alpha* ((alpha-values &rest fun-defs)
			  &body body)
  `(let* (,@(loop for (fun . lambda-list) in fun-defs
	       append (apply #'build-binding-list-1 alpha-values
			      fun lambda-list)))
     ,@body))

(define-test with-f-alpha
  (assert-expands
   '(let ((f-1 (f x-1 y))
	  (f-2 (f x-2 y))
	  (f-3 (f x-3 y))
	  (g-1 (g x-1 y z-1))
	  (g-2 (g x-2 y z-2))
	  (g-3 (g x-3 y z-3)))
     t)
   (with-f-alpha ((1 2 3) (f x-alpha y) (g x-alpha y z-alpha)) t))
    (assert-expands
   '(let ((f-1 (f x-1 y))
	  (f-2 (f x-2 y))
	  (f-3 (f x-3 y)))
     t)
   (with-f-alpha-1 ((1 2 3) (f x-alpha y)) t)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun build-binding-list-2 (alpha-values beta-values fun &rest lambda-list)
    "Build a binding list for fun and lambda-list args
FUN - symbol
LAMBDA-LIST - symbols also
ALPHA-VALUES - index list"
    (let ((fun-name (symbol-name fun)))
      (loop for alpha in alpha-values
	 for beta in beta-values
	 collect
	   (let ((fun-alpha-beta
		  (symbolicate fun-name
			       "-" (format nil "~a" alpha)
			       "-" (format nil "~a" beta)))
		 (args-alpha-beta
		  (loop for arg in lambda-list
		     collect
		     (intern (subst-alpha-beta
			      (symbol-name arg) alpha beta)))))
	     `(,fun-alpha-beta (,fun ,@args-alpha-beta)))))))

(define-test build-binding-list-2
  (assert-equal '((f-1-3 (f x-1 y-3))
		  (f-2-2 (f x-2 y-2))
		  (f-3-1 (f x-3 y-1)))
		(build-binding-list-2 '(1 2 3) '(3 2 1)
				      'f 'x-alpha 'y-beta)))

#|(defmacro with-f-alpha-beta ((alpha-values beta-values
					   (fun &rest lambda-list))
			     &body body)
  `(let (,@(apply #'build-binding-list-2 alpha-values
		  beta-values fun lambda-list))
     ,@body))


(define-test with-f-alpha-beta
  (assert-expands
   '(let ((f-1-3 (f x-1 y-3))
	  (f-2-2 (f x-2 y-2))
	  (f-3-1 (f x-3 y-1)))
     t)
   (with-f-alpha-beta ((1 2 3) (3 2 1) (f x-alpha y-beta)) t)))|#

(defmacro with-f-alpha-beta ((alpha-values beta-values
					   &rest fun-defs)
			     &body body)
  `(let (,@(loop for (fun . lambda-list) in fun-defs
	      append (apply #'build-binding-list-2 alpha-values
			    beta-values fun lambda-list)))
     ,@body))

(defmacro with-f-alpha-beta* ((alpha-values beta-values
					   &rest fun-defs)
			     &body body)
  `(let* (,@(loop for (fun . lambda-list) in fun-defs
	      append (apply #'build-binding-list-2 alpha-values
			    beta-values fun lambda-list)))
     ,@body))

(define-test with-f-alpha-beta
  (assert-expands
   '(let ((f-1-3 (f x-1 y-3))
	  (f-2-2 (f x-2 y-2))
	  (f-3-1 (f x-3 y-1))
	  (g-1-3 (g x-1 y-3))
	  (g-2-2 (g x-2 y-2))
	  (g-3-1 (g x-3 y-1)))
     t)
   (with-f-alpha-beta ((1 2 3) (3 2 1)
			 (f x-alpha y-beta)
			 (g x-alpha y-beta)) t))
  (assert-expands
   '(let ((f-1-3 (f x-1 y-3))
	  (f-2-2 (f x-2 y-2))
	  (f-3-1 (f x-3 y-1)))
     t)
   (with-f-alpha-beta-1 ((1 2 3) (3 2 1) (f x-alpha y-beta)) t)))

(defmacro defun-w-bindings (alpha-list beta-list fun args &body fun-body)
  "Define function `fun' of `args' with `body'.  Using the
`alpha-list' and `beta-list', define a bind- macro that will create a
local binding to all possible variations.


As an example,

 (defun-w-bindings M*-ALPHA-BETA (1 2) (2 1) (M-ALPHA M-BETA)
   (/ (* m-alpha m-beta) (+ m-alpha m-beta)))

will result in the following being done:

Function definition
 (DEFUN M*-ALPHA-BETA (M-ALPHA M-BETA)
  (/ (* M-ALPHA M-BETA) (+ M-ALPHA M-BETA)))

A bind-m*-alpha-beta form
  (let ((m*-1-2 (m*-alpha-beta m-1 m-2))
        (m*-2-1 (m*-alpha-beta m-2 m-1)))
     ,@body)
"
  (let* ((fun-name (symbol-name fun))
	 (core-name (first (split-sequence #\= fun-name)))
;;	 (calc-fun-name (symbolicate "CALC-" fun-name))
	 (bind-vars-name (symbolicate "BIND-" fun-name))
	 (bindings
	  (loop for alpha in alpha-list
	     for beta in beta-list
	     collect (let ((dsm-name (symbolicate (subst-alpha-beta core-name alpha beta)))
			   (exp-args (loop for arg in args
					collect (symbolicate (subst-alpha-beta (symbol-name arg) alpha beta)))))
		       `'(,dsm-name (,fun ,@exp-args))))))
    (unless core-name
      (error "Function name has to end with a ="))
    `(progn
       (defun ,fun ,args
	 ,@fun-body)
       (defmacro ,bind-vars-name (&body body)
	 `(let (,,@bindings)
	   ,@body)))))


(define-test defun-w-bindings
  (assert-expands
   '(PROGN
     (DEFUN M*-ALPHA-BETA (M-ALPHA M-BETA)
       (/ (* M-ALPHA M-BETA) (+ M-ALPHA M-BETA)))
     (DEFMACRO BIND-M*-ALPHA-BETA (&BODY BODY)
       (CONS 'LET
	     (CONS (LIST '(M*-1-1 (M*-ALPHA-BETA M-1 M-1))
			 '(M*-1-2 (M*-ALPHA-BETA M-1 M-2))
			 '(M*-2-1 (M*-ALPHA-BETA M-2 M-1))
			 '(M*-2-2 (M*-ALPHA-BETA M-2 M-2)))
		   BODY))))
   (defun-w-bindings (1 1 2 2) (1 2 1 2) M*-ALPHA-BETA (M-ALPHA M-BETA)
     (/ (* m-alpha m-beta) (+ m-alpha m-beta))))
    (assert-expands
     '(PROGN
     (DEFUN M*-ALPHA-BETA (M-ALPHA M-BETA)
       (/ (* M-ALPHA M-BETA) (+ M-ALPHA M-BETA)))
     (DEFMACRO BIND-M*-ALPHA-BETA (&BODY BODY)
      (CONS 'LET
	    (CONS (LIST '(M*-1-2 (M*-ALPHA-BETA M-1 M-2))
			'(M*-2-1 (M*-ALPHA-BETA M-2 M-1)))
		  BODY))))
   (defun-w-bindings (1 2) (2 1) M*-ALPHA-BETA (M-ALPHA M-BETA)
     (/ (* m-alpha m-beta) (+ m-alpha m-beta)))))