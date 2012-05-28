;; Mirko Vukovic
;; Time-stamp: <2012-05-27 13:47:46 index-sugar-package-def.lisp>
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

(defpackage :index-sugar
  (:use #:common-lisp #:split-sequence #:lisp-unit
	#:anaphora
	#:symbol-processing
	#:infix2prefix)
  (:import-from :alexandria
		:symbolicate)
  (:export :with-f-alpha-beta
	   :with-f-alpha
	   :with-f-alpha-beta*
	   :with-f-alpha*
	   :with-_-indexing
	   :with-_-indexing-1
	   :bind-alpha-beta-vars)
  (:documentation
"Package for easing typing and reading code with many indexed variables.

See the README.org file for general documentation.  More documentation
is in the asdf file, the function documentation and unit-tests"))