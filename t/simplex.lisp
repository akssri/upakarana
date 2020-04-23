;; Copyright (c) 2020 Akshay Srinivasan <akssri@vakra.xyz>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package #:upakarana-tests)

(5am:def-suite :simplex :in test-suite)
(5am:in-suite :simplex)

(defparameter *simplex-lp-test-problems*
  '((#2A((1 0 1 -1  -1  3)
	 (0 1 2 -1 -1/2 1))
     #(0 0)
     #(0 0 -1 -1 -3 8))
    ;;
    (#2A((1 -1  -1  3 1 0)
	 (2 -1 -1/2 1 0 1))
     #(0 0)
     #(-1 -1 -3 8 0 0))
    ;;
    (#2A((1 1 0 1)
	 (2 0 1 0))
     #(1 1)
     #(-1 1 -2 0))
    ;;
    (#2A((1  1 2 1 3 -1  0)
	 (2 -2 3 1 1  0 -1))
     #(4 3)
     #(2 3 5 2 3 0 0))
    ;;
    (#2A(( 1  8  0  -1  0  0  0)
	 (-3 -7 -20  0 -1  0  0)
	 ( 1  0  0   0  0 -1  0)
	 ( 0 -1  0   0  0  0 -1)
	 ( 1  1  1   0  0  0  0))
     #(3 -6 2/5 -1/2 1)
     #(5 2 1/4 0 0 0 0))
    ;;
    (#2A((1 2  1  1 -2)
	 (1 1 -4 -2 -3)
	 (1 2  5 -4  6))
     #(4 2 3)
     #(3 5 4 5 6))
    ;;
    (#2A((1  2 -1  1)
	 (2 -2  3  3)
	 (1 -1  2 -1))
     #(0 9 6)
     #(-3 1 3 -1))
    ;;
    (#2A((-1  0  0  0  0 -1 1 0 0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0)
	 ( 0 -1  0  0  0  0 0 1 0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0)
	 (-1  1  0 -1 -1  1 0 0 1  0  0 -1  0  0  0  0  0  0  0  0  0  0  0)
	 ( 1  0  0  1  0  0 0 0 0  0  0  0 -1  0  0  0  0  0  0  0  0  0  0)
	 ( 1 -1  0  0  0  0 0 0 0  0  0  0  0 -1  0  0  0  0  0  0  0  0  0)
	 ( 0  1 -2  0  0  0 0 0 0  0  0  0  0  0 -1  0  0  0  0  0  0  0  0)
	 ( 0  0  1 -1  0  0 0 0 0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0)
	 ( 0  0  1  0 -1  0 0 0 0  0  0  0  0  0  0  0 -1  0  0  0  0  0  0)
	 ( 0  0  0  1  0 -1 0 0 0  0  0  0  0  0  0  0  0 -1  0  0  0  0  0)
	 ( 0  0  0  0  1  0 0 0 0  0  0  0  0  0  0  0  0  0 -1  0  0  0  0)
	 ( 0  0  0  0  1  0 0 0 0  0  0  0  0  0  0  0  0  0  0 -1  0  0  0)
	 ( 0  0  0  0  0  1 0 0 0  0  0  0  0  0  0  0  0  0  0  0 -1  0  0)
	 ( 0  0  0  0  0  0 0 0 0  0  0  0  1  0  0  0  0  0  0  0  0 -1  0)
	 ( 0  0  0  0  0  0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  1  0 -1)
	 )
     #(0 0 0 0 0 0 0 0 0 0 0 0 1 1)
     #(0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(defun run-simplex (prob)
  (letv* (((A b c) prob)
	  (A-sparse (iter :l0 (for ii below (array-dimension A 0))
			  (iter (for jj below (array-dimension A 1))
				(if (/= (aref A ii jj) 0)
				    (in :l0 (collect (list* ii jj (aref A ii jj)))))))))
    (handler-case (u.splx:linprog c A-sparse (make-array (length b) :initial-element :=) b)
      (u.splx:simplex-infeasible () :inf)
      (u.splx:simplex-unbounded () :unb))))

(5am:test simplex-test
  (is (equal
       (mapcar #'run-simplex *simplex-lp-test-problems*)
       '(:UNB :UNB -2 5 219/68 11 7 0))))
