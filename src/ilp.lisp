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

(in-package #:upakarana-simplex)

(defun branch-heuristic (x integer-variables &optional (eps (scale-float 1d0 -46)))
  (iter (for ii in-vector integer-variables with-index pp)
	(letv* ((xi (aref x ii)) (xi-int (round xi)))
	  (unless (< (abs (- xi xi-int)) eps)
	    (finding pp minimizing (abs (- (+ 0.5d0 xi-int) xi)))))))

(defun branch-tableau (j ub-j x-j x-lb cvec tableau)
  (with-slots (n-slack n-artificial A b A-basic.t^{-1} row-basic) tableau
    (letv* ((flx-j (coerce (floor x-j) 'simplex-dtype)))
      (when (< 0 (aref b ub-j 0))
	(remove nil
	 (list
	  (let ((new-b (copy-array b)))
	    (setf (aref new-b ub-j 0) flx-j)
	    (list cvec x-lb (make-instance 'simplex-tableau :A A :b new-b
					   :A-basic.t^{-1} (copy-array A-basic.t^{-1})
					   :row-basic (copy-array row-basic))))
	  (when (< (1+ flx-j) (aref b ub-j 0))
	    (letv* ((new-b (copy-array b)) (cvec (copy-array cvec))
		    (x-lb (copy-array x-lb)))
	      (iter (for ii in-vector (csc-matrix-idx A) from (aref (csc-matrix-ptr A) j) below (aref (csc-matrix-ptr A) (1+ j)) with-index pp)
		    (decf (aref new-b ii 0) (* (aref (csc-matrix-val A) pp) (1+ flx-j))))
	      (incf (aref x-lb j) (1+ flx-j))
	      (incf (aref cvec (1- (length cvec))) (* (aref cvec j) (1+ flx-j)))
	      (list cvec x-lb (make-instance 'simplex-tableau :A A :b new-b
					     :A-basic.t^{-1} (copy-array A-basic.t^{-1})
					     :row-basic (copy-array row-basic)))))))))))

(defun intlinprog (c a op b &key integer-constraint (max-iterations-simplex 100) (max-iterations-branch-bound 100) &aux (tableau (make-feasible-tableau A op b)))
  (letv* ((integer-variables (coerce integer-constraint '(simple-array simplex-itype (*))))
	  (ub-constraint (tableau-upper-bound-constraints integer-variables tableau))
	  (stack nil) (min-solution nil)
	  (%max-iterations-branch-bound max-iterations-branch-bound))
    ;; initialize stack
    (letv* ((cvec (make-cvec c tableau)))
      (push (list cvec (make-array (1- (length cvec)) :element-type 'simplex-dtype) tableau #'primal-simplex-step) stack))
    ;; branch-bound
    (iter (while stack)
      (if (>= nbranch %max-iterations-branch-bound)
	  (restart-case (error 'simplex-exceeded-max-iterations :count max-iterations-branch-bound :tableau (and min-solution (fourth min-solution)))
	    (continue () (incf %max-iterations-branch-bound max-iterations-branch-bound))))
      ;;
      (letv* (((cvec x-lb tableau &optional (step-function #'dual-simplex-step)) (pop stack))
	      (copt bx bl (handler-case (simplex-solve cvec tableau max-iterations-simplex step-function)
				(u.splx:simplex-infeasible () (next-iteration))))
	      (xopt lopt (simplex-solution bx bl tableau x-lb)))
	(when (or (not min-solution) (< copt (first min-solution)))
	  (if-let (branch-p (branch-heuristic xopt integer-variables))
	    ;; non-integer solution, branch
	    (letv* ((branch-j (aref integer-variables branch-p))
		    (branches (branch-tableau branch-j (aref ub-constraint branch-p) (aref xopt branch-j) x-lb cvec tableau)))
	      (setf stack (nconc branches stack)))
	    ;; integer solution
	    (setf min-solution (list copt xopt lopt tableau)))))
      (counting t into nbranch))
    ;; return
    (if (not min-solution) (error 'simplex-infeasible :tableau tableau)
	(values-list min-solution))))

;;
(defun tableau-upper-bound-constraints (integer-variables tableau)
  (if (= 0 (length integer-variables)) (make-array 0 :element-type 'simplex-itype)
      (with-slots (A n-slack n-artificial) tableau
	(letv* ((n-total (csc-matrix-n A))
		(ptr (csc-matrix-ptr A)) (idx (csc-matrix-idx A)) (val (csc-matrix-val A))
		(adjt (make-array (csc-matrix-m A) :initial-element nil))
		(ub-index (make-array (length integer-variables) :element-type 'simplex-itype)))
	  (iter (for jj from (- n-total n-artificial 1) downto 0)
		(iter (for ii in-vector idx from (aref ptr jj) below (aref ptr (1+ jj)) with-index pp)
		      (push (cons jj (aref val pp)) (aref adjt ii))))
	  ;;
	  (flet ((ubidx (jj)
		   (iter (for ii in-vector idx from (aref ptr jj) below (aref ptr (1+ jj)) with-index pp)
			 (finding ii such-that (and (= (aref val pp) 1)
						    (letv* ((adj-ii (aref adjt ii)))
						      ;; (list _ (cons (>= (- n-total n-slack n-artificial)) . (= 1)))
						      (and (cdr adj-ii) (null (cddr adj-ii))
							   (letv* (((j1 . v1) (second adj-ii)))
							     (and (>= j1 (- n-total n-slack n-artificial)) (= v1 1))))))))))
	    (iter (for jj in-vector integer-variables with-index pp)
		  (let ((ub-jj (ubidx jj)))
		    (if ub-jj (setf (aref ub-index pp) ub-jj)
			(error "upper bound constraint not found for variable ~a" jj))))
	    ub-index)))))
