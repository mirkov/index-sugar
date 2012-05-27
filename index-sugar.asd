;; Mirko Vukovic
;; Time-stamp: <2012-05-25 23:02:53 index-sugar.asd>
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

(asdf:defsystem index-sugar
  :name "index-sugar"
  :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
  :version "0.1"
  :serial t
  :components
  ((:module "package-setup"
	    :pathname #p"./"
	    :components
	    ((:file "index-sugar-package-def")))
   (:module "utilities"
	    :pathname #p"./"
	    :depends-on ("package-setup")
	    :components
	    (;; special variables
	     (:file "setup")
	     ;; string and symbol manipulation
	     (:file "helper-functions")
	     ))
   (;; See README.org for brief description
    :module "f-alpha-beta"
	    :pathname #p"./"
	    :depends-on ("utilities")
	    :components
	    ((:file "f-alpha-beta")))
   (;; see README.org for brief description
    :module "array-indexing"
	    :pathname #p"./"
	    :depends-on ("utilities")
	    :components
	    ((:file "array-indexing"))))
  :depends-on ("split-sequence"
	       "alexandria"
	       "lisp-unit"
	       "cl-ppcre"
	       "anaphora"
	       "symbol-name-processing"
	       "infix2prefix"))