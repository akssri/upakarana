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

(deftype simplex-dtype () 'rational)
(deftype simplex-itype () 'fixnum)

(define-condition simplex-infeasible (error)
  ((infeasible-row :initarg :infeasible-row)
   (tableau :initarg :tableau)))

(define-condition simplex-unbounded (error)
  ((unbounded-variables :initarg :unbounded-variables)
   (tableau :initarg :tableau)))

(define-condition simplex-exceeded-max-iterations (error)
  ((count :initarg :count)
   (tableau :initarg :tableau)))

;;
(defstruct csc-matrix
  (m 0 :type simplex-itype)
  (n 0 :type simplex-itype)
  (ptr (make-array 0 :element-type 'simplex-itype) :type (simple-array simplex-itype (*)))
  (idx (make-array 0 :element-type 'simplex-itype) :type (simple-array simplex-itype (*)))
  (val (make-array 0 :element-type 'simplex-dtype) :type (simple-array simplex-dtype (*))))

(defun dense-csc-gemm! (alpha A B col-B beta C)
  "C^T <- alpha A^T B[:, col-B] + beta C^T,
   where B is a sparse csc-matrix."
  (if (/= beta 1) (<- (_ii _jj) (aref C _ii _jj) (* beta (aref C _ii _jj))))
  (let ((ptr (csc-matrix-ptr B))
	(idx (csc-matrix-idx B))
	(val (csc-matrix-val B)))
    (loop :for jj-b :across col-B
       :do (loop :for ptr-kk :from (aref ptr jj-b) :below (aref ptr (1+ jj-b))
	      :do (let ((kk (aref idx ptr-kk))
			(s (* alpha (aref val ptr-kk))))
		    (<- (_ii) (aref C jj-c _ii) (+ (aref C jj-c _ii) (* s (aref A kk _ii))))))
       :counting t :into jj-c))
  C)

(defun gemm! (alpha A b beta c &optional transpose-A)
  "C <- alpha op(A) B + beta C"
  (if (/= beta 1) (<- (_ii _jj) (aref c _ii _jj) (* beta (aref c _ii _jj))))
  (if (not transpose-A)
      (loop :for ii :from 0 :below (array-dimension A 0)
	 :do (loop :for kk :from 0 :below (array-dimension A 1)
		:do (let ((s (* alpha (aref A ii kk))))
		      (<- (_jj) (aref c ii _jj) (+ (aref c ii _jj) (* s (aref b kk _jj)))))))
      (error "not implemented"))
  c)

(defun gemv! (alpha A b beta c &optional transpose-A)
  "C <- op(A) B + beta C"
  (if (/= beta 1) (<- (_ii) (aref c _ii) (* beta (aref c _ii))))
  (if (not transpose-A)
      (loop :for ii :from 0 :below (array-dimension A 0)
	 :do (let ((acc 0))
	       (loop :for _kk :from 0 :below (array-dimension A 1)
		  :do (incf acc (* (aref A ii _kk) (aref b _kk))))
	       (setf (aref c ii) (+ (aref c ii) (* alpha acc)))))
      (loop :for kk :from 0 :below (array-dimension A 0)
	 :do (let ((s (* alpha (aref b kk))))
	       (<- (_ii) (aref c _ii) (+ (aref c _ii) (* s (aref A kk _ii)))))))
  c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
  revised simplex
  ---------------
  canonical form for simplex the tableau,
  min c x
      A x = b, b >= 0, A \in \mathbb{R}^{(m, n)}, m <= n
      x >= 0

  identify basic variable set B of size m; non-basic set R = {1..n} \ B

  A_R x_R + A_B x_B = b
  => x_b = A_B^{-1} (b - A_R x_R)

  supp. A_B^{-1} b >= 0, so that,
  (x_B, x_R) = (A_B^{-1} b, 0)
  is a feasible solution.

  cost = c_B A_B^{-1} b + (c_R - c_B A_B^{-1} A_R) x_R

  let w_R = (c_R - c_B A_B^{-1} A_R)

  if \exists i, (w_R)_i < 0, then there is scope for further decrease of the cost function along x_{R[i]}.
  else, return (c_B A_B^{-1} b) as opt. value.

  let x_j be the non-basic variable with the steepest negative gradient (bland's rule).

  rewriting constraint eqn.,
  x_B + A_B^{-1} A_R x_R = A_B^{-1} b           >= 0 (by assumption)

  let M = A_B^{-1} A_R; M_j the column correspoding to x_j
  let p = A_B^{-1} b

  therefore,
  x_B + M_{-j} x_{R-j} + M_j x_j = p            >= 0 (by assumption)

  if M_j <= 0, then the LP is unbounded since x_j -> -\infty is feasible.
  else, find row which minimizes max(p/M_j, 0),

  r = argmin_i max(p/M_j, 0)[i]

  setting x_j = max(p/M_j, 0)[r] maintains feasibility.
  add 'j' into set of basic variables, and pop basic variable corresponding to 'r'.
  update A_B^{-1}.

  repeat.
|#

(defclass simplex-tableau ()
  ((n-slack :initform 0 :initarg :n-slack)
   (n-artificial :initform 0 :initarg :n-artificial)
   (A :initarg :A) (b :initarg :b)
   (A-basic.t^{-1} :initarg :A-basic.t^{-1})
   (row-basic :initarg :row-basic) (col-basic :initarg :col-basic)))

(defun simplex-pivot! (Bt^-1 r Mr)
  "(PIVOT! Bt^-1 r Mr)
   update @arg{Bt^-1} such that the @arg{r}'th column of B was replaced by B @arg{Mr}.

   the O(n^2) update is computed by pivoting on the r-th row of the following tableau,
   [I ; M_r | B^{-1}]
  "
  (loop :for ii :from 0 :below (array-dimension Bt^-1 0)
     :do (if (/= ii r)
	   (let ((s (/ (aref Mr 0 ii) (aref Mr 0 r))))
	     (<- (_jj) (aref Bt^-1 _jj ii) (- (aref Bt^-1 _jj ii) (* s (aref Bt^-1 _jj r)))))))
  (let ((s (/ (aref Mr 0 r))))
    (<- (_jj) (aref Bt^-1 _jj r) (* s (aref Bt^-1 _jj r))))
  Bt^-1)

(defun simplex-cost (c tableau &optional (n (length c)))
  (with-slots (A b A-basic.t^{-1} row-basic col-basic) tableau
    ;;non-basic cb cb.Btinv cnb
    (letv* ((m (length b))
	    ;; enumerate non-basic variables
	    (non-basic
	     (iter (for bv-jj in-vector col-basic below n with-index jj)
	       (unless bv-jj (collect jj result-type 'vector))))
	    ;; allocate
	    (c-basic (make-array (list m 1) :element-type 'simplex-dtype))
	    (c-basic.A-basic^{-1} (make-array (list m 1) :element-type 'simplex-dtype))
	    (cr-non-basic (make-array (list (length non-basic) 1) :element-type 'simplex-dtype)))
      ;; setup c_{R}; (m, 1)
      (iter (for nbv-ii in-vector non-basic with-index ii)
	(setf (aref cr-non-basic ii 0) (aref c nbv-ii)))
      ;; setup c_{B}; (m, 1)
      (iter (for bv-ii in-vector row-basic with-index ii)
	(setf (aref c-basic ii 0) (aref c bv-ii)))
      ;; c_{B}^{T} A_{B}^{-1}; (m, 1)
      (gemm! 1 A-basic.t^{-1} c-basic 0 c-basic.A-basic^{-1})
      (values
       ;; c_{B}^{T} A_{B}^{-1} b
       (loop :for ii :below m :summing (* (aref b ii) (aref c-basic.A-basic^{-1} ii 0)))
       ;; c_{R} - c_{B}^{T} A_{B}^{-1} A_{R}
       (dense-csc-gemm! -1 c-basic.A-basic^{-1} A non-basic 1 cr-non-basic)
       ;; non-basic variable indices
       non-basic))))

(defun simplex-column (jj tableau)
  (with-slots (A b A-basic.t^{-1}) tableau
    (letv* ((m (length b)))
      (values-n (if jj 2 1)
       ;; A_{B}^{-1} b; (m,)
       (gemv! 1 A-basic.t^{-1} b 0 (make-array (list m) :element-type 'simplex-dtype) t)
       ;; A_{B}^{-1} A[:, jj]; (1, m)
       (dense-csc-gemm! 1 A-basic.t^{-1} A (vector jj) 0 (make-array (list 1 m) :element-type 'simplex-dtype))))))

(defun simplex-solve (c tableau &optional (n (length c)) (max-iterations 100))
  (with-slots (A-basic.t^{-1} row-basic col-basic non-basic) tableau
    (iter (repeat max-iterations)
      (letv* ((cost-feasible cr-non-basic non-basic (simplex-cost c tableau n))
	      (pivot-col (iter (for nb-ii in-vector non-basic with-index ii)
			   (let ((cr-i (aref cr-non-basic ii 0))) ;;bland's rule
			     (if (< cr-i 0) (finding nb-ii maximizing (- cr-i))))))
	      (nil (unless pivot-col (return cost-feasible)))
	      (A-basic^{-1}.b M-pivot-col (simplex-column pivot-col tableau))
	      (pivot-row
	       (iter (for ii from 0 below (array-dimension M-pivot-col 1))
		 (if (< 0 (aref M-pivot-col 0 ii))
		     (finding ii minimizing (/ (aref A-basic^{-1}.b ii) (aref M-pivot-col 0 ii)))))))
	(if (not pivot-row)
	    (error 'simplex-unbounded :unbounded-variables (list pivot-col) :tableau tableau)
	    (progn
	      (simplex-pivot! A-basic.t^{-1} pivot-row M-pivot-col)
	      (setf (aref col-basic (aref row-basic pivot-row)) nil
		    (aref row-basic pivot-row) pivot-col
		    (aref col-basic pivot-col) pivot-row))))
      (finally (restart-case (error 'simplex-exceeded-max-iterations :count max-iterations :tableau tableau)
		 (continue (answer) (when answer (simplex-solve c tableau n max-iterations))))))))

(defun linprog (c A op b &key (max-iterations 100) (tableau (make-tableau A op b)))
  "(LINPROG c A op b &key (max-iterations 100))
   solve the LP,
   min   <c, x>, st. x >= 0,  A x op b
  "
  (with-slots (row-basic col-basic n-artificial) tableau
    (let ((n-total (length col-basic)))
      ;;phase-1
      (let ((c-feasible (make-array n-total :element-type 'simplex-dtype)))
	(iter (for rv in-vector row-basic) (setf (aref c-feasible rv) (coerce 1 'simplex-dtype)))
	(unless (= 0 (simplex-solve c-feasible tableau (length c-feasible) max-iterations))
	  (error 'simplex-infeasible :tableau tableau)))
      ;;phase-2
      (letv* ((c-opt (make-array n-total :element-type 'simplex-dtype))
	      (nil (<- ((ii 0 (length c))) (aref c-opt ii) (coerce (aref c ii) 'simplex-dtype)))
	      (opt (simplex-solve c-opt tableau (- n-total n-artificial) max-iterations))
	      (x (simplex-column nil tableau))
	      (ret (make-array (- n-total n-artificial) :initial-element (coerce 0 'simplex-dtype) :element-type 'simplex-dtype)))
	(iter (for ri in-vector row-basic with-index ii) (setf (aref ret ri) (aref x ii)))
	(values opt ret tableau)))))

;;tableau initialization;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun canonicalize-constraints (n b op)
  (letv* ((row-basic (make-array (length b) :initial-element nil :fill-pointer t :adjustable t))
	  (n-slack -1) (n-artificial -1) (A-slack nil)
	  (new-b (let ((new-b (make-array (length b) :element-type 'simplex-dtype)))
		   (<- (_ii) (aref new-b _ii) (coerce (aref b _ii) 'simplex-dtype)))))
    (iter (for op-ii in-vector op with-index ii)
      ;; set b_i <- |b_i|
      (when (< (aref new-b ii) 0)
	(setf (aref new-b ii) (- (aref new-b ii))))
      ;; add slack & ←artificial variables
      (ecase op-ii
	((:=)
	 ;; artificial slack variable v_i used for finding initial feasible solution (phase-1)
	 ;; A_{i,:} * x = 0 -> A_{i,:} * x + v_i = b_i,  v_i >= 0       (b_i >= 0)
	 (push (list* ii (list :artificial (incf n-artificial)) (if (<= 0 (aref b ii)) 1 -1)) A-slack))
	((:<= :>=)
	 (let ((sign (ecase op-ii (:<= 1) (:>= -1))))
	   ;; A_{i,:} * x <= b_i -> A_{i,:} * x + s_i = b_i,  s_i >= 0, (b_i >= 0)
	   (push (list* ii (list :slack (incf n-slack)) sign) A-slack)
	   ;; the slack variable can be used as the basic variable when sign * b_i >= 0
	   (unless (<= 0 (* sign (aref b ii)))
	     ;; else add artificial variable
	     ;; if b_i < 0 -> -A_{i,:} * x - s_i + v_i = -b_i
	     (push (list* ii (list :artificial (incf n-artificial)) (- sign)) A-slack)))))
      ;; set basic variable for row ii
      (setf (aref row-basic ii) (second (first A-slack))))
    (incf n-slack) (incf n-artificial)
    ;;
    (flet ((index (idx &optional type) (+ idx (case type (:slack n) (:artificial (+ n n-slack)) (t 0)))))
      ;; compute offsets
      (iter (for entry in A-slack)
	(letv* (((_ (type idx) . _) entry))
	  (setf (second entry) (index idx type))))
      ;; basic-variable index
      (iter (for bv-ii in-vector row-basic with-index ii)
	(letv* (((type idx) bv-ii))
	  (setf (aref row-basic ii) (index idx type)))))
    ;;
    (values A-slack new-b (coerce row-basic '(simple-array simplex-itype (*))) n-slack n-artificial)))

(defun build-adj-list (coo)
  (letv* ((adj (make-extensible-vector))
	  (m n nptr (iter (for (ii jj . vv) in coo)
		      (when (/= vv 0)
			(iter (while (>= jj (length adj))) (vector-push-extend nil adj))
			(push (cons ii vv) (aref adj jj))
			(counting t into nptr)
			(maximizing ii into m))
		      (finally (return (values (1+ m) (length adj) nptr))))))
    (values m n nptr adj)))

(defun build-csc-matrix (m n nptr adj row-sign)
  (letv* ((ptr (make-array (1+ n) :element-type 'simplex-itype))
	  (idx (make-array (1+ nptr) :element-type 'simplex-itype))
	  (val (make-array (1+ nptr) :element-type 'simplex-dtype)))
    (iter (for col.jj in-vector adj with-index jj) (with ll = 0)
      (iter (for (ii . vv) in (sort col.jj #'< :key #'first))
	(setf (aref idx ll) ii
	      (aref val ll) (* (if (<= 0 (aref row-sign ii)) 1 -1) vv))
	(incf ll))
      (setf (aref ptr (1+ jj)) ll))
    (make-csc-matrix :m m :n n :ptr ptr :idx idx :val val)))

(defun make-tableau (A op b)
  (letv* ((m n nptr A-adj (build-adj-list A))
	  (nil (assert (= m (length b)) nil "matrix size mismatch"))
	  (A-slack new-b row-basic n-slack n-artificial (canonicalize-constraints n b op))
	  ;; update adj-list
	  (nil (iter (for (ii jj . vv) in A-slack)
		 (when (/= vv 0)
		   (iter (while (>= jj (length A-adj))) (vector-push-extend nil A-adj))
		   (push (cons ii vv) (aref A-adj jj)))))
	  (A-csc (build-csc-matrix m (+ n n-slack n-artificial) (+ nptr n-slack n-artificial) A-adj b))
	  (Btinv (make-array (list m m) :element-type 'simplex-dtype))
	  (col-basic (make-array (csc-matrix-n A-csc) :initial-element nil)))
    ;; setup col-basic and Btinv
    (iter (for bv-ii in-vector row-basic with-index ii)
      (setf (aref col-basic bv-ii) ii
	    (aref Btinv ii ii) (/ (aref (csc-matrix-val A-csc) (aref (csc-matrix-ptr A-csc) bv-ii)))))
    (make-instance 'simplex-tableau
		   :n-slack n-slack :n-artificial n-artificial
		   :A A-csc :b new-b :A-basic.t^{-1} Btinv
		   :row-basic row-basic :col-basic col-basic)))
