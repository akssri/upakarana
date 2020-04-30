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

(deftype simplex-dtype () 'double-float)
(deftype simplex-itype () 'fixnum)

;; linear algebra routines
(defstruct csc-matrix
  (m 0 :type simplex-itype)
  (n 0 :type simplex-itype)
  (ptr (make-array 0 :element-type 'simplex-itype) :type (simple-array simplex-itype (*)))
  (idx (make-array 0 :element-type 'simplex-itype) :type (simple-array simplex-itype (*)))
  (val (make-array 0 :element-type 'simplex-dtype) :type (simple-array simplex-dtype (*))))

(defun dense-csc-gemm! (alpha A B col-B beta C)
  "C^T <- alpha A^T B[:, col-B] + beta C^T,
   where B is a sparse csc-matrix."
  (declare (type (simple-array simplex-dtype (* *)) A C)
	   (type (simple-array simplex-itype (*)) col-B)
	   (type csc-matrix B))
  (assert (and (iter (for ci in-vector col-B) (always (<= 0 ci (1- (csc-matrix-n B)))))
	       (= (array-dimension A 1) (array-dimension C 1))
	       (= (array-dimension A 0) (csc-matrix-m B))
	       (= (length col-B) (array-dimension C 0)))
	  nil "invalid matrix dimensions for MM")
  (let ((alpha (coerce alpha 'simplex-dtype)) (beta (coerce beta 'simplex-dtype)))
    (declare (type simplex-dtype alpha beta)
	     (optimize (speed 3) (safety 0)))
    (if (/= beta 1) (<- (_ii _jj) (aref C _ii _jj) (* beta (aref C _ii _jj))))
    (let ((ptr (csc-matrix-ptr B))
	  (idx (csc-matrix-idx B))
	  (val (csc-matrix-val B)))
      (loop :for jj-b :across col-B
	    :do (loop :for ptr-kk :from (aref ptr jj-b) :below (aref ptr (1+ jj-b))
		      :do (let ((kk (aref idx ptr-kk))
				(s (* alpha (aref val ptr-kk))))
			    (<- (_ii) (aref C jj-c _ii) (+ (aref C jj-c _ii) (* s (aref A kk _ii))))))
	    :counting t :into jj-c)))
  C)

(defun gemm! (alpha A B beta C &optional transpose-A)
  "C <- alpha op(A) B + beta C"
  (declare (type (simple-array simplex-dtype (* *)) A B C))
  (assert (if (not transpose-A)
	      (and (= (array-dimension A 0) (array-dimension C 0))
		   (= (array-dimension A 1) (array-dimension B 0))
		   (= (array-dimension B 1) (array-dimension C 1)))
	      (and (= (array-dimension A 1) (array-dimension C 0))
		   (= (array-dimension A 0) (array-dimension B 0))
		   (= (array-dimension B 1) (array-dimension C 1))))
	  nil "invalid matrix dimensions for MM")
  (let ((alpha (coerce alpha 'simplex-dtype)) (beta (coerce beta 'simplex-dtype)))
    (declare (type simplex-dtype alpha beta)
	     (optimize (speed 3) (safety 0)))
    (if (/= beta 1) (<- (_ii _jj) (aref c _ii _jj) (* beta (aref c _ii _jj))))
    (if (not transpose-A)
	(loop :for ii :from 0 :below (array-dimension A 0)
	      :do (loop :for kk :from 0 :below (array-dimension A 1)
			:do (let ((s (* alpha (aref A ii kk))))
			      (<- (_jj) (aref c ii _jj) (+ (aref c ii _jj) (* s (aref b kk _jj)))))))
	(loop :for ii :from 0 :below (array-dimension A 1)
	      :do (loop :for kk :from 0 :below (array-dimension A 0)
			:do (let ((s (* alpha (aref A kk ii))))
			      (<- (_jj) (aref c ii _jj) (+ (aref c ii _jj) (* s (aref b kk _jj)))))))))
  c)

(defun inverse-update! (Bt^-1 r Mr)
  "(INVERSE-UPDATE! Bt^-1 r Mr)
   update @arg{Bt^-1} such that the @arg{r}'th column of B was replaced by B @arg{Mr}.

   the O(n^2) update is computed by pivoting on the r-th row of the following tableau,
   [I ; M_r | B^{-1}]
  "
  (declare (type simplex-itype r)
	   (type (simple-array simplex-dtype (* *)) Bt^-1 Mr))
  (assert (and (= (array-dimension Bt^-1 0) (array-dimension Bt^-1 1) (array-dimension Mr 1))
	       (= (array-dimension Mr 0) 1) (<= 0 r (1- (array-dimension Bt^-1 0))))
	  nil "invalid matrix dimensions for inverse update")
  (locally (declare (optimize (speed 3) (safety 0)))
    (loop :for ii :from 0 :below (array-dimension Bt^-1 0)
	  :do (if (/= ii r)
		  (let ((s (/ (aref Mr 0 ii) (aref Mr 0 r))))
		    (<- (_jj) (aref Bt^-1 _jj ii) (- (aref Bt^-1 _jj ii) (* s (aref Bt^-1 _jj r)))))))
    (let ((s (/ (aref Mr 0 r))))
      (<- (_jj) (aref Bt^-1 _jj r) (* s (aref Bt^-1 _jj r)))))
  Bt^-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition simplex-infeasible (error)
  ((infeasible-rows :initarg :infeasible-rows)
   (tableau :initarg :tableau)))

(define-condition simplex-unbounded (error)
  ((unbounded-variables :initarg :unbounded-variables)
   (tableau :initarg :tableau)))

(define-condition simplex-exceeded-max-iterations (error)
  ((count :initarg :count)
   (tableau :initarg :tableau)))

(defclass simplex-tableau ()
  ((n-slack :initform 0 :initarg :n-slack)
   (n-artificial :initform 0 :initarg :n-artificial)
   (A :initarg :A) (b :initarg :b)
   (A-basic.t^{-1} :initarg :A-basic.t^{-1})
   (row-basic :initarg :row-basic :documentation "b_i: (i, x_{b_i}) is basic.")
   (col-basic :initarg :col-basic :documentation "c_j: if c_j >=0, (c_j, x_j) is basic; else, x_j is the (-c_j-1, x_j) is non-basic")))

(defun simplex-pivot! (pivot-row pivot-col M-pivot-col tableau)
  "(SIMPLEX-PIVOT! pivot-row pivot-col M-pivot-col tableau)
   set @arg{pivot-col} as the @arg{pivot-row}'th basic variable,
   -  update row-basic col-basic
   -  update A-basic.t^{-1} with @arg{M-pivot-col} (= A_{B}^{-1} A[:, pivot-col])"
  (with-slots (A b A-basic.t^{-1} row-basic col-basic non-basic) tableau
    (inverse-update! A-basic.t^{-1} pivot-row M-pivot-col)
    (rotatef (aref col-basic (aref row-basic pivot-row)) (aref col-basic pivot-col))
    (setf (aref row-basic pivot-row) pivot-col))
  nil)

(defun simplex-state (c tableau)
  "(SIMPLEX-STATE c tableau) => cost-feasible d-non-basic x-feasible λ-optimal
			     => (c_{B}^{T} A_{B}^{-1} b, c_{R} - c_{B}^{T} A_{B}^{-1} A_{R}, A_{B}^{-1} b, c_{B}^{T} A_{B}^{-1})"
  (declare (type (simple-array simplex-dtype (*)) c)
	   (type simplex-tableau tableau))
  (with-slots (A b A-basic.t^{-1} row-basic col-basic) tableau
    (letv* ((m (csc-matrix-m A)) (n (1- (length c)))
	    (c-basic (make-array (list m 1) :element-type 'simplex-dtype))
	    (non-basic (make-array (- n m) :initial-element -1 :element-type 'simplex-itype))
	    (d-non-basic (make-array (list (- n m) 1) :element-type 'simplex-dtype))
	    (c-basic.A-basic^{-1} (make-array (list m 1) :element-type 'simplex-dtype)))
      ;; setup non-basic; (n-m,)
      (iter (for nbv-ii below n)
	(when (< (aref col-basic nbv-ii) 0)
	  (setf (aref non-basic ii) nbv-ii)
	  (counting t into ii)))
      ;; setup c_{R}; (m, 1)
      (iter (for nbv-ii in-vector non-basic with-index ii)
	(assert (<= 0 nbv-ii (1- n)) nil "noo")
	(setf (aref d-non-basic ii 0) (aref c nbv-ii)))
      ;; setup c_{B}; (m, 1)
      (iter (for bv-ii in-vector row-basic with-index ii)
	(assert (<= 0 bv-ii (1- n)) nil "noo")
	(setf (aref c-basic ii 0) (aref c bv-ii)))
      ;; c_{B}^{T} A_{B}^{-1}; (m, 1)
      (gemm! 1 A-basic.t^{-1} c-basic 1 c-basic.A-basic^{-1})
      (values
       ;; c_{B}^{T} A_{B}^{-1} b
       (+ (aref c n) (loop :for ii :below m :summing (* (aref b ii 0) (aref c-basic.A-basic^{-1} ii 0))))
       ;; c_{R} - c_{B}^{T} A_{B}^{-1} A_{R}; (|R|, 1)
       (dense-csc-gemm! -1 c-basic.A-basic^{-1} A non-basic 1 d-non-basic)
       ;; A_{B}^{-1} b; (m, 1); x at feasibility
       (gemm! 1 A-basic.t^{-1} b 1 (make-array (list m 1) :element-type 'simplex-dtype) t)
       ;; c_{B}^{T} A_{B}^{-1}; (m, 1); \lambda at optimality
       c-basic.A-basic^{-1}
       ;; non-basic
       non-basic))))

;;
#|
  primal simplex
  --------------
  canonical form for simplex,
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

  let d_R = (c_R - c_B A_B^{-1} A_R)

  if \exists i, (d_R)_i < 0, then there is scope for further decrease of the cost function along x_{R[i]}.
  else, return (c_B A_B^{-1} b) as opt. value.

  let x_j be the non-basic variable with the steepest negative gradient (bland's rule).

  rewriting constraint eqn.,
  x_B + A_B^{-1} A_R x_R = A_B^{-1} b           >= 0 (by assumption)

  let M = A_B^{-1} A_R; M_j the column correspoding to x_j
  let p = A_B^{-1} b

  therefore,
  x_B + M_{-j} x_{R-j} + M_j x_j = p            >= 0 (by assumption)

  if M_j <= 0, then the LP is unbounded since x_j -> -\infty is feasible.
  else, find row which minimizes p/M_j > 0

  r = argmin_i max(p/M_j, 0)[i]

  setting x_j = max(p/M_j, 0)[r] achieves best improvement while maintaining feasibility.

  add 'j' into set of basic variables, and pop basic variable corresponding to 'r'.
  update A_B^{-1}.

  repeat.
|#

(defun primal-direction (jj tableau)
  "(PRIMAL-DIRECTION jj tableau) => A_{B}^{-1} A[:, jj]"
  (with-slots (A A-basic.t^{-1}) tableau
    ;; A_{B}^{-1} A[:, jj]; (1, m)
    (dense-csc-gemm! 1 A-basic.t^{-1} A (make-array 1 :initial-element jj :element-type 'simplex-itype) 1 (make-array (list 1 (array-dimension A-basic.t^{-1} 0)) :element-type 'simplex-dtype))))

(defun primal-simplex-step (c tableau)
  "(PRIMAL-SIMPLEX-STEP c tableau)
   assumption: primal feasibility (dual optimality),
	       A_B^{-1} b >= 0"
  (with-slots (A-basic.t^{-1} row-basic col-basic) tableau
    (letv* ((cost-feasible d-non-basic A-basic^{-1}.b c-basic.A-basic^{-1} non-basic (simplex-state c tableau))
	    (pivot-col (iter (for nb-jj in-vector non-basic with-index jj)
			 (let ((cr-jj (aref d-non-basic jj 0))) ;;bland's rule
			   (if (< cr-jj 0) (finding nb-jj maximizing (- cr-jj)))))))
      (if (not pivot-col) (list cost-feasible A-basic^{-1}.b c-basic.A-basic^{-1}) ;;optimality
	  (letv* ((M-pivot-col (primal-direction pivot-col tableau))
		  (pivot-row (iter (for ii from 0 below (array-dimension M-pivot-col 1))
			       (if (< 0 (aref M-pivot-col 0 ii))
				   (finding ii minimizing (/ (aref A-basic^{-1}.b ii 0) (aref M-pivot-col 0 ii)))))))
	    (if (not pivot-row) (error 'simplex-unbounded :unbounded-variables (list pivot-col) :tableau tableau)
		(simplex-pivot! pivot-row pivot-col M-pivot-col tableau)))))))

;;
(defun simplex-solve (c tableau &optional (max-iterations 100))
  (iter (repeat max-iterations)
    (if-let (ret (primal-simplex-step c tableau))
      (return (values-list ret)))
    (finally (restart-case (error 'simplex-exceeded-max-iterations :count max-iterations :tableau tableau)
	       (continue () (simplex-solve c tableau max-iterations))))))

(defun linprog (c A op b &key (max-iterations 100) &aux (tableau (make-tableau A op b)))
  "(LINPROG c A op b &key [max-iterations 100]) => cost-optimal, primal, dual, tableau

   solve the LP,
   min   <c, x> := c_0 x_0 + c_1 x_1 ..... c_{n-1} x_{n-1} + c_{n},
	 st.   x >= 0,
	     A x op b
   if solution exists returns, (c^*, x, λ, tableau)
   else one of the following conditions will be raised,
   - SIMPLEX-INFEASIBLE
   - SIMPLEX-UNBOUNDED
   - SIMPLEX-EXCEEDED-MAX-ITERATIONS (restart: CONTINUE)"
  (with-slots (row-basic col-basic n-artificial) tableau
    (let ((n-total (length col-basic)))
      ;;phase-1
      (when (> n-artificial 0)
	(let ((c-feasible (make-array (1+ n-total) :element-type 'simplex-dtype)))
	  (iter (for ii below n-artificial) (setf (aref c-feasible (- n-total ii 1)) (coerce 1 'simplex-dtype)))
	  (unless (= 0 (simplex-solve c-feasible tableau max-iterations))
	    (error 'simplex-infeasible :tableau tableau))))
      ;;phase-2
      (letv* ((c-opt (make-array (1+ (- n-total n-artificial)) :element-type 'simplex-dtype))
	      (nil (progn (<- ((ii 0 (1- (length c)))) (aref c-opt ii) (coerce (aref c ii) 'simplex-dtype))
			  (setf (aref c-opt (1- (length c-opt))) (coerce (aref c (1- (length c))) 'simplex-dtype))))
	      (opt x lambda (simplex-solve c-opt tableau max-iterations))
	      (primal (make-array (- n-total n-artificial) :element-type 'simplex-dtype))
	      (dual (make-array (length row-basic) :element-type 'simplex-dtype)))
	(iter (for ri in-vector row-basic with-index ii) (setf (aref primal ri) (aref x ii 0)))
	(<- (ii) (aref dual ii) (aref lambda ii 0))
	(values opt primal dual tableau)))))

;;
#|
  dual simplex
  ------------
  primal canonical form,
  min c x
      A x = b, b >= 0, A \in \mathbb{R}^{(m, n)}, m <= n
      x >= 0

  with the corresponding dual,
  max b' y
      A' y + w = c'
      w >= 0

  identify basic variable set B of size m; non-basic set R = {1..n} \ B,

  A_B x_B + A_R x_R = b
  => A' y + w = [A_B' y + w_B; A_R' y + w_R]

  A' y + w = c'
  => [A_B', I, 0] [y  ]    [c_B']
     [A_R', 0, I] [w_B]  = [c_R']
		  [w_R]

  consider the submatrix of y and w_R,
  W = [A_B', 0]
      [A_R', I]
  W^{-1} = [A_B'^{-1},       0]
	   [-A_R' A_B'^{-1}, I]

  - if the basis W is feasible for the dual then,
  [y  ] = [A_B'^{-1},       0] [c_B']
  [w_R]   [-A_R' A_B'^{-1}, I] [c_R']
  => -A_R' A_B'^{-1} c_B' + c_R' >= 0
  <=> primal optimality

  - if the basic W is optimal for the dual then,
  (-x_B) = 0 - [b', 0] [A_B'^{-1},       0] [I] = -b' A_B'^{-1} <= 0
		       [-A_R' A_B'^{-1}, I] [0]
  <=> primal feasibility

  supp. the dual is feasible.

  if \exists i, (x_B)_i < 0, then there is scope for further increase of the dual objective along w_{B[i]}.
  else, return (c_B A_B^{-1} b) as the opt. value.

  let w_j be the basic-slack variable with the steepest negative gradient (bland's rule).

  rewriting constraint eqn. for the dual for w_R,
  0 <= w_R = d_R' + G_{-j} w_{R-j} + G_j w_j,
  where,
  G = A_R' A_B'^{-1}

  if G_j >= 0, then the dual is unbounded since w_j -> \infty is dual-feasible.
  else, find index which minimizes -d_R'/G_j > 0.

  pivot & repeat.
|#

(defun dual-direction (ii non-basic tableau)
  "(DUAL-DIRECTION ii tableau) => A_{B}^{-1}[ii, :] A[:, jj]"
  (with-slots (A A-basic.t^{-1}) tableau
    (let ((m (array-dimension A-basic.t^{-1} 0)))
      (letv* ((A-basic.t^{-1}_ii (make-array (list m 1) :element-type 'simplex-dtype)))
	(<- (jj) (aref A-basic.t^{-1}_ii jj 0) (aref A-basic.t^{-1} jj ii))
	(dense-csc-gemm! 1 A-basic.t^{-1}_ii A non-basic 1 (make-array (list (length non-basic) 1) :element-type 'simplex-dtype))))))

(defun dual-simplex-step (c tableau)
  "(DUAL-SIMPLEX-STEP c tableau)
   assumption: dual feasibility (primal optimality),
	       (c_R - c_B A_B^{-1} A_R) >= 0"
  (with-slots (A-basic.t^{-1} row-basic col-basic) tableau
    (letv* ((cost-feasible d-non-basic A-basic^{-1}.b c-basic.A-basic^{-1} non-basic (simplex-state c tableau))
	    (pivot-row (iter (for ii below (length row-basic))
			 (let ((beta-ii (aref A-basic^{-1}.b ii 0))) ;;bland's rule
			   (if (< beta-ii 0) (finding ii maximizing (- beta-ii)))))))
      (if (not pivot-row) (list cost-feasible A-basic^{-1}.b c-basic.A-basic^{-1}) ;;feasibility
	  (letv* ((G-pivot-row (dual-direction pivot-row non-basic tableau))
		  (pivot-col (iter (for nbv-jj in-vector non-basic with-index jj)
			       (if (> 0 (aref G-pivot-row jj 0))
				   (finding nbv-jj minimizing (/ (- (aref d-non-basic jj 0)) (aref G-pivot-row jj 0)))))))
	    (if (not pivot-col) (error 'simplex-infeasible :infeasible-rows (list pivot-row) :tableau tableau) ;; unbounded dual
		(simplex-pivot! pivot-row pivot-col (primal-direction pivot-col tableau) tableau)))))))

;;tableau initialization;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun canonicalize-constraints (n b op)
  (letv* ((row-basic (make-array (length b) :initial-element nil :fill-pointer t :adjustable t))
	  (n-slack -1) (n-artificial -1) (A-slack nil)
	  (new-b (let ((new-b (make-array (list (length b) 1) :element-type 'simplex-dtype)))
		   (<- (_ii) (aref new-b _ii 0) (abs (coerce (aref b _ii) 'simplex-dtype))))))
    (iter (for op-ii in-vector op with-index ii)
      ;; add slack & artificial variables
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
	      (aref val ll) (coerce (* (if (<= 0 (aref row-sign ii)) 1 -1) vv) 'simplex-dtype))
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
	  (col-basic (make-array (csc-matrix-n A-csc) :initial-element -1 :element-type 'simplex-itype)))
    ;; setup Btinv
    (iter (for bv-ii in-vector row-basic with-index ii)
      (setf (aref Btinv ii ii) (/ (aref (csc-matrix-val A-csc) (aref (csc-matrix-ptr A-csc) bv-ii)))))
    ;; col-basic
    (iter (for bv-ii in-vector row-basic with-index ii)
      (setf (aref col-basic bv-ii) ii))
    ;;
    (make-instance 'simplex-tableau
		   :n-slack n-slack :n-artificial n-artificial
		   :A A-csc :b new-b :A-basic.t^{-1} Btinv
		   :row-basic row-basic :col-basic col-basic)))

(defun tableau-feasiblep (tableau)
  (with-slots (A-basic.t^{-1} b row-basic col-basic n-artificial) tableau
    (if (= n-artificial 0) t
	(and (not (iter (for bv in-vector row-basic) (thereis (<= (- (length col-basic) n-artificial) bv))))
	     (letv* ((m (length row-basic))
		     (x-basic (gemm! 1 A-basic.t^{-1} b 1 (make-array (list m 1) :element-type 'simplex-dtype) t)))
	       (iter (for ii below m) (always (<= 0 (aref x-basic ii 0)))))))))
