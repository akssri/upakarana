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
  '((#(0 0 -1 -1 -3 8 0)
     #2A((1 0 1 -1  -1  3)
	 (0 1 2 -1 -1/2 1))
     #(:= :=)
     #(0 0))
    ;;

    (#(-1 -1 -3 8 0 0 0)
     #2A((1 -1  -1  3 1 0)
	 (2 -1 -1/2 1 0 1))
     #(:= :=)
     #(0 0))
    ;;
    (#(-1 1 -2 0 0)
     #2A((1 1 0 1)
	 (2 0 1 0))
     #(:= :=)
     #(1 1))
    ;;
    (#(2 3 5 2 3 0 0)
     #2A((1  1 2 1 3 -1  0)
	 (2 -2 3 1 1  0 -1))
     #(:= :=)
     #(4 3))
    ;;
    (#(5 2 1/4 0 0 0 0 0)
     #2A(( 1  8  0  -1  0  0  0)
	 (-3 -7 -20  0 -1  0  0)
	 ( 1  0  0   0  0 -1  0)
	 ( 0 -1  0   0  0  0 -1)
	 ( 1  1  1   0  0  0  0))
     #(:= := := := :=)
     #(3 -6 2/5 -1/2 1))
    ;;
    (#(3 5 4 5 6 0)
     #2A((1 2  1  1 -2)
	 (1 1 -4 -2 -3)
	 (1 2  5 -4  6))
     #(:= := :=)
     #(4 2 3))
    ;;
    (#(-3 1 3 -1 0)
     #2A((1  2 -1  1)
	 (2 -2  3  3)
	 (1 -1  2 -1))
     #(:= := :=)
     #(0 9 6))
    ;;
    (#(0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
     #2A((-1  0  0  0  0 -1 1 0 0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0)
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
	 ( 0  0  0  0  0  0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  1  0 -1))
     #(:= := := := := := := := := := := := := :=)
     #(0 0 0 0 0 0 0 0 0 0 0 0 1 1))
    ;;
    (#(1 2 5/10 2/10 1 -6/10 10)          ;; min x_1 + 2 x_2 + 0.5 x_3 + 0.2 x_4 + x_5 - 0.6 x_6 + 10
     ((0 0 . 1) (0 1 . 2)                 ;; x_1 + 2 x_2 >= 1
      (1 0 . 1) (1 1 . 1) (1 5 . 3)       ;; x_1 + x_2 + 3 x_6 >= 1
      (2 0 . 1) (2 1 . 1) (2 5 . 1)       ;; x_1 + x_2 + x_6 >= 1
      (3 2 . 1) (3 3 . -3)                ;; x_3 - 3 x_4 >= 1
      (4 2 . 1) (4 3 . -2) (4 4 . -5)     ;; x_3 - 2 x_4 - 5 x_5 >= 1
      (5 3 . 1) (5 4 . 3) (5 5 . -4)      ;; x_4 + 3 x_5 - 4 x_6 >= 1
      (6 1 . 1) (6 4 . 1) (6 5 . 1)       ;; x_2 + x_5 + x_6 >= 1
      (7 0 . 1) (8 1 . 1) (9 2 . 1)       ;; 0 <= x_1, x_2, x_3, x_4, x_5, x_6 <= 10
      (10 3 . 1) (11 4 . 1) (12 5 . 1))
     #(:>= :>= :>= :>= :>= :>= :>= :<= :<= :<= :<= :<= :<=)
     #(1 1 1 1 1 1 1 10 10 10 10 10 10))))

(defun run-simplex (prob)
  (letv* (((c A op b) prob)
	  (A-sparse (typecase A
		      (list A)
		      ((simple-array * 2)
		       (iter (for ii below (array-dimension A 0))
			 (dotimes (jj (array-dimension A 1))
			   (if (/= (aref A ii jj) 0)
			       (collect (list* ii jj (aref A ii jj))))))))))
    (handler-case (u.splx:linprog c A-sparse op b)
      (u.splx:simplex-infeasible () :inf)
      (u.splx:simplex-unbounded () :unb))))

(5am:test simplex-test
  (let ((eps 1d-13))
    (is (every #'(lambda (a b)
		   (etypecase b
		     (symbol (eql a b))
		     (number (< (abs (- a b)) eps))))
	       (mapcar #'run-simplex *simplex-lp-test-problems*)
	       (list :UNB :UNB -2 5 219/68 11 7 0 (+ 10 10/3))))))


(5am:test branch-bound-test
  (letv* ((eps (scale-float 1d0 -46))
	  ((c a op b) '(#(1 2 5/10 2/10 1 -6/10 0)
			((0 1 . 2) (0 0 . 1) ;; x_1 + 2 x_2
			 (1 5 . 3) (1 1 . 1) (1 0 . 1) ;; x_1 + x_2 + 3 x_6
			 (2 5 . 1) (2 1 . 1) (2 0 . 1) ;; x_1 + x_2 + x_6
			 (3 3 . -3) (3 2 . 1) ;; x_3 - 3 x_4
			 (4 4 . -5) (4 3 . -2) (4 2 . 1) ;; x_3 - 2 x_4 - 5 x_5
			 (5 5 . -4) (5 4 . 3) (5 3 . 1) ;; x_4 + 3 x_5 - 4 x_6
			 (6 5 . 1) (6 4 . 1) (6 1 . 1) ;; x_2 + x_5 + x_6
			 (12 5 . 1) (11 4 . 1) (10 3 . 1) (9 2 . 1) (8 1 . 1) (7 0 . 1))
			#(:>= :>= :>= :>= :>= :>= :>= :<= :<= :<= :<= :<= :<=)
			#(1 1 1 1 1 1 1 10 10 10 10 10 10)))
	  (integer-variables (coerce (vector 0 1 2 3 4 5) '(simple-array u.splx:simplex-itype (*)))))
    (is (< (abs (- (u.splx:intlinprog c a op b :integer-constraint integer-variables) 42/10)) eps))))
