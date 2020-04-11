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

(in-package #:upakarana-fourier-motzkin)

;; Implementation derived from Pluto
(deftype inequation-dtype () 't)

(defstruct inequation
  (row (make-array 0 :element-type 'inequation-dtype) :type (simple-array inequation-dtype (*)))
  (op ':<= :type keyword))

(define-condition fourier-motzkin-infeasible (error) ())

(defun simplify-constraints! (constraints)
  ;;normalize
  (iter (for cst.i in-vector constraints with-index i)
	(let* ((a.i (inequation-row cst.i))
	       (op.i (inequation-op cst.i))
	       (nz -1) (ncount 0)
	       (gcd (iter (for a.ij in-vector a.i with-index j)
			  (when (/= 0 a.ij)
			    (if (< j (1- (length a.i))) (incf ncount)) ;; check for constant inequation
			    (if (< nz 0) (setf nz j))                  ;; canonicalize by first nz variable
			    (reducing a.ij by #'gcd initial-value 0)))))
	  (assert (or (< 0 ncount)
		      (ecase op.i
			(:= nil)
			(:<= (<= 0 (aref a.i (1- (length a.i)))))
			(:>= (>= 0 (aref a.i (1- (length a.i)))))))
		  nil 'fourier-motzkin-infeasible)
	  (cond
	    ((or (= 0 gcd) (= 0 ncount)) (setf (aref constraints i) nil))
	    (t (let ((s (* gcd (ecase op.i (:>= -1) (:<= 1) (:= (signum (aref a.i nz)))))))
		 (if (< s 0) (setf (inequation-op cst.i) (ecase op.i (:<= :>=) (:>= :<=) (:= :=))))
		 (if (/= s 1) (<- (_jj) (aref a.i _jj) (floor (aref a.i _jj) s))))))))
  ;;sort constraints
  (letv* ((constraints (remove nil constraints))
	  (order (sort (let ((idx (make-array (length constraints) :element-type 'fixnum)))
			 (loop :for i :from 0 :below (length idx) :do (setf (aref idx i) i))
			 idx)
		       #'(lambda (m n)
			   (iter (for a.mj in-vector (inequation-row (aref constraints m)))
				 (for a.nj in-vector (inequation-row (aref constraints n)))
				 (if (/= a.mj a.nj) (return (< a.mj a.nj)))
				 (finally (return (eql (inequation-op (aref constraints m))
						       (inequation-op (aref constraints n))))))))))
    ;;remove redundancies
    (iter (for n in-vector order) (with m = -1)
	  (if (< m 0) (setf m n)
	      (let* ((a.m (inequation-row (aref constraints m)))
		     (a.n (inequation-row (aref constraints n)))
		     (ncols (length a.m)))
		(iter (for a.mj in-vector a.m below (1- ncols))
		      (for a.nj in-vector a.n)
		      (when (/= a.mj a.nj) (setf m n) (return))
		      (finally (when (eql (inequation-op (aref constraints m))
					  (inequation-op (aref constraints n)))
				 (setf (aref a.m (1- ncols)) (min (aref a.m (1- ncols)) (aref a.n (1- ncols)))
				       (aref constraints n) nil)))))))
    (remove nil constraints)))

(defun fm-pivot! (row col constraints)
  (let* ((cst.row (aref constraints row))
	 (a.row (inequation-row cst.row))
	 (r (aref a.row col)))
    (iter (for cst.ii in-vector constraints with-index ii)
	  (when (/= ii row)
	    (let* ((a.ii (inequation-row cst.ii)))
	      (when (/= 0 (aref a.ii col))
		(let* ((gcd (gcd (abs r) (abs (aref a.ii col))))
		       (s.ii (floor (abs r) gcd)) (s.row (* -1 (signum r) (floor (aref a.ii col) gcd))))
		  (<- (_jj) (aref a.ii _jj) (+ (* s.ii (aref a.ii _jj)) (* s.row (aref a.row _jj))))))))))
  constraints)

(defun merge-constraints (var cst.m cst.n)
  ;;(print (list cst.m cst.n))
  (letv* ((a.m (inequation-row cst.m)) (a.n (inequation-row cst.n))
	  (op.m (inequation-op cst.m)) (op.n (inequation-op cst.n))
	  (lcm (lcm (abs (aref a.m var)) (abs (aref a.n var))))
	  (s.m (* (ecase op.m (:<= 1) (:>= -1)) (floor lcm (abs (aref a.m var)))))
	  (s.n (* (ecase op.n (:<= 1) (:>= -1)) (floor lcm (abs (aref a.n var)))))
	  (ret (make-array (length a.m) :element-type (array-element-type a.m))))
    (<- (_jj) (aref ret _jj) (+ (* s.m (aref a.m _jj)) (* s.n (aref a.n _jj))))
    (make-inequation :row ret :op ':<=)))

(defun fourier-motzkin (col constraints)
  (letv* ((ub (make-array (length constraints) :element-type 'fixnum :fill-pointer 0))
	  (lb (make-array (length constraints) :element-type 'fixnum :fill-pointer 0))
	  (nb (make-array (length constraints) :element-type 'fixnum :fill-pointer 0))
	  (eq (make-array (length constraints) :element-type 'fixnum :fill-pointer 0)))
    ;;mark constraints, check for equalities
    (let ((row (iter (for cst.ii in-vector constraints with-index ii)
		     (let ((op.ii (inequation-op cst.ii))
			   (a.ij (aref (inequation-row cst.ii) col)))
		       (cond ((= a.ij 0) (vector-push ii nb))
			     ((eql op.ii :=) (vector-push ii eq)
			      (finding ii maximizing (abs a.ij)))
			     ((< a.ij 0) (vector-push ii (ecase op.ii (:<= ub) (:>= lb))))
			     (t (vector-push ii (ecase op.ii (:<= lb) (:>= ub)))))))))
      (if row
	  (letv* ((nil (fm-pivot! row col constraints))
		  (ret (concatenate 'vector (subseq constraints 0 row) (subseq constraints (1+ row)))))
	    (values (subseq constraints row (1+ row)) ret ;;(simplify-constraints! ret)
		    ))
	  (letv* ((ret (make-array 0 :fill-pointer t :adjustable t))
		  (sol (make-array 0 :fill-pointer t :adjustable t)))
	    (iter (for kk in-vector nb) (vector-push-extend (aref constraints kk) ret))
	    (iter (for lb.ii in-vector lb with-index ii)
		  (iter (for ub.jj in-vector ub with-index jj)
			(vector-push-extend (merge-constraints col (aref constraints lb.ii) (aref constraints ub.jj)) ret)))
	    ;;setup solution
	    (iter (for ii in-vector lb) (vector-push-extend (aref constraints ii) sol))
	    (iter (for ii in-vector ub) (vector-push-extend (aref constraints ii) sol))
	    (values sol (simplify-constraints! ret)))))))

;; TODO - Add tests.
#+nil
(letv* ((A (vector (make-inequation :row (coerce #(1 0 10) '(simple-array inequation-dtype (*))) :op :<=)
		   (make-inequation :row (coerce #(1 0 0) '(simple-array inequation-dtype (*))) :op :>=)
		   (make-inequation :row (coerce #(0 1 5) '(simple-array inequation-dtype (*))) :op :<=)
		   (make-inequation :row (coerce #(0 1 0) '(simple-array inequation-dtype (*))) :op :>=)
		   (make-inequation :row (coerce #(1 -1 0) '(simple-array inequation-dtype (*))) :op :<=))))
  (fourier-motzkin 1 A))
