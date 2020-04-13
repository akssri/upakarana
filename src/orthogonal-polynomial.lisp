;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10 -*-
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

(in-package #:upakarana-orthogonal-polynomial)

;; orthogonal polynomial
(defclass orthogonal-polynomial ()
  ((an :initarg :an)
   (bn :initarg :bn)
   (cn :initarg :cn)
   (p0 :initform 1 :initarg :p0)
   (v0 :initarg :v0)
   (vn :initform nil :initarg :vn))
  (:documentation
   "P_{n} = (a_n x + b_n) P_{n - 1} - c_n P_{n - 2}; P_{-1} = 0
    v_0 = (P_0, P_0)_w"))

(defmacro make-orthogonal-polynomial (&rest body)
  (with-gensyms (ret)
    (let ((slots (mapcan #'(lambda (x)
			     (letv* (((name . rest) x))
			       (case name
				 ((:an :bn :cn)
				  (letv* ((((n) &rest body) rest))
				    `(,name (lambda (,n) (declare (ignorable ,n))
					      (assert (<= 1 ,n) nil "coefficient ~a only for n >= 1" name)
					      (cond ,@body)))))
				 ((:p0 :v0)
				  (letv* (((value) rest))
				    `(,name ,value))))))
			 body)))
      `(let ((,ret (make-instance 'orthogonal-polynomial ,@slots)))
	 (assert (every #'(lambda (_ss) (slot-boundp ,ret _ss)) '(an bn cn p0 v0 vn)) nil "missing slots")
	 ,ret))))
;;

(with-memoization ()
  (defun orthogonal-polynomial (name &rest args)
    (case name
      (:chebyshev-t
       ;;domain: [-1, 1]
       ;;weight: {1 \over \sqrt(1 - x^2) }
       (make-orthogonal-polynomial
	(:an (n)
	     ((= 1 n) 1)
	     ((<= 2 n) 2))
	(:bn (n) (t 0))
	(:cn (n) (t 1))
	(:p0 1) (:v0 pi)))
      (:chebyshev-u
       ;;domain: [-1, 1]
       ;;weight: \sqrt(1 - x^2)
       (make-orthogonal-polynomial
	(:an (n) ((<= 1 n) 2))
	(:bn (n) (t 0))
	(:cn (n) (t 1))
	(:p0 1) (:v0 (/ pi 2))))
      (:legendre
       ;;domain: [-1, 1]
       ;;weight: 1
       (make-orthogonal-polynomial
	(:an (n)
	     ((= n 1) 1)
	     (t (- 2 (/ 1 n))))
	(:bn (n) (t 0))
	(:cn (n) (t (- 1 (/ 1 n))))
	(:p0 1) (:v0 2)))
      (:laguerre
       ;;domain: [0, \infty)
       ;;weight: x^{alpha} \exp(-x)
       (letv* (((&optional (alpha 0)) args))
	 (make-orthogonal-polynomial
	  (:an (n) ((<= 1 n) (- (/ n))))
	  (:bn (n) ((<= 1 n) (+ 2 (/ (1- alpha) n))))
	  (:cn (n) ((<= 2 n) (+ 1 (/ (1- alpha) n))))
	  (:p0 1) (:v0 1))))
      (:hermite-e
       ;;domain: (-\infty, \infty)
       ;;weight: \exp(-x^2 / 2)
       (make-orthogonal-polynomial
	(:an (n) (t 1))
	(:bn (n) (t 0))
	(:cn (n) (t (1- n)))
	(:p0 1) (:v0 (sqrt (* 2 pi)))))
      (:hermite
       ;;domain: (-\infty, \infty)
       ;;weight: \exp(-x^2)
       (make-orthogonal-polynomial
	(:an (n) (t 2))
	(:bn (n) (t 0))
	(:cn (n) (t (* 2 (1- n))))
	(:p0 1) (:v0 (sqrt pi)))))))
;;
(defun evaluate-orthopoly (x n p &optional (n-grad 0))
  (with-slots (an bn cn p0) p
    (let* ((dp (make-array (list (1+ n-grad) 2) :initial-element 0)))
      ;; setup
      (if (= 0 n)
	  (setf (aref dp 0 1) p0)
	  (progn
	    (setf (aref dp 0 0) p0
		  (aref dp 0 1) (* p0 (+ (* (funcall an 1) x) (funcall bn 1))))
	    (if (<= 1 n-grad)
		(setf (aref dp 1 1) (* p0 (funcall an 1))))))
      ;; recurrence
      (iter (for j from 2 below n)
	(let ((aj (funcall an j)) (bj (funcall bn j)) (cj (funcall cn j)))
	  (iter (for i from 0 to n-grad)
	    (let ((dpi+ (+ (* (aref dp i 1) (+ (* aj x) bj)) (if (< 0 i) (* i aj (aref dp (1- i) 0)) 0) (* (- cj) (aref dp i 0)))))
	      (setf (aref dp i 0) (aref dp i 1)
		    (aref dp i 1) dpi+)))))
      ;; return
      (values-list (iter (for ii below (1+ n-grad)) (collect (aref dp ii 1)))))))

(defun lagrange-derivative-matrix (x)
  (let* ((n-knots (length x))
	 (ret (make-array (list n-knots n-knots))))
    (dotimes (n n-knots)
      (dotimes (i n-knots)
	(setf (aref ret n i)
	      (if (= n i)
		  (iter (for k from 0 below n-knots)
		    (summing (if (= k i) 0 (/ 1 (- (aref x i) (aref x k))))))
		  (iter (for j from 0 below n-knots)
		    (multiply (/ (if (or (= j n) (= j i)) 1 (- (aref x n) (aref x j)))
				 (if (= j i) 1 (- (aref x i) (aref x j))))))))))
    ret))
;; normalized recurrence
(defun alpha-n (n p)
  "x P_n = \alpha_n P_{n - 1} + \beta_{n + 1} P_{n} + \alpha_{n + 1} P_{n + 1},
   where P_n are orthonormal.

   The recurrence relation for the orthonormal family can be obtained using the following correspondence,
   \alpha_n = \sqrt{c_{n + 1} \over a_{n + 1} a_n}.
   \beta_n = {-b_n \over a_n}."
  (with-slots (an bn cn) p
    (cond
      #+nil
      ((<= 1 n) (* (sqrt (/ (norm-square n p) (norm-square (1- n) p)))
		   (/ (funcall (slot-value p 'an) n))))
      ((<= 1 n) (sqrt (/ (funcall cn (1+ n))
			 (funcall an (1+ n))
			 (funcall an n)
			 1d0)))
      (t (error "alpha-n only defined for n >= 1")))))

(defun beta-n (n p)
    "x P_n = \alpha_n P_{n - 1} + \beta_{n + 1} P_{n} + \alpha_{n + 1} P_{n + 1},
     where P_n are orthonormal.

     The recurrence relation for the orthonormal family can be obtained using the following correspondence,
     \alpha_n = \sqrt{c_{n + 1} \over a_{n + 1} a_n}.
     \beta_n = {-b_n \over a_n}."
  (with-slots (an bn cn) p
    (cond
      ((<= 1 n) (* -1 (funcall bn n) (/ (funcall an n))))
      (t (error "beta-n only defined for n >= 1")))))

(defun norm-square (n p &optional (v0 (slot-value p 'v0)))
  "(P_n, P_n)_w"
  (with-slots (an bn cn vn) p
    (labels ((vn (n)
	       (cdr (assoc! n vn :default
		 (cond
		   ((= n 0) 1)
		   ((< 0 n) (/ (* (funcall cn (1+ n)) (funcall an n) (vn (1- n)))
			       (funcall an (1+ n)))))))))
      (* (vn n) v0))))
;;
(defun gauss-quadrature (n p)
  "Computes knot points and quadrature weights by diagonalizing the Jacobi operator associated with the orthogonal polynomial [1,2],

   [1] Golub, Gene H., and John H. Welsch. Calculation of Gauss Quadrature rules. Mathematics of computation 23.106 (1969): 221-230.
   [2] Srinivasan, Akshay. Spectral Methods: Applications to Quantum Mechanics and Flow Stability. B.Tech thesis (2011), NITK, Surathkal"
  (let* ((dn (make-array n :initial-element 0d0 :element-type 'double-float))
	 (en (make-array n :initial-element 0d0 :element-type 'double-float))
	 (zn (make-array n :initial-element 0d0 :element-type 'double-float)))
    ;; setup
    (dotimes (ii n)
      (setf (aref dn ii) (coerce (beta-n (1+ ii) p) 'double-float))
      (if (< 0 ii) (setf (aref en (- ii 1)) (coerce (alpha-n ii p) 'double-float))))
    (setf (aref zn 0) 1d0)
    ;; golub-welsch
    (imtqlx dn en zn)
    (with-slots (v0 p0) p
      (let ((zz (/ v0 (* p0 p0))))
	(<- (ii) (aref zn ii) (* zz (aref zn ii) (aref zn ii)))))
    (values dn zn)))
;;
(defmacro %for (init condition iteration &rest body)
  `(progn
     ,init
     (loop :while ,condition :do
       (progn ,@body)
	   :do ,iteration)))

(defun imtqlx (d e z &optional (max-iterations 30) &aux (n (length d)) (prec 2.220446049250313d-16))
  "
  Purpose:

    IMTQLX diagonalizes a symmetric tridiagonal matrix.

  Discussion:

    This routine is a slightly modified version of the EISPACK routine to
    perform the implicit QL algorithm on a symmetric tridiagonal matrix.

    The authors thank the authors of EISPACK for permission to use this
    routine.

    It has been modified to produce the product Q' * Z, where Z is an input
    vector and Q is the orthogonal matrix diagonalizing the input matrix.
    The changes consist (essentially) of applying the orthogonal transformations
    directly to Z as they are generated.

  Licensing:

    This code is distributed under the GNU GPL license.

  Modified:

    13 April 2020

  Author:

    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
    C version (& documentation) by John Burkardt.

    Lisp version (derived from f2cl) by Akshay Srinivasan.

  Reference:

    Sylvan Elhay, Jaroslav Kautsky,
    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of
    Interpolatory Quadrature,
    ACM Transactions on Mathematical Software,
    Volume 13, Number 4, December 1987, pages 399-415.

    Roger Martin, James Wilkinson,
    The Implicit QL Algorithm,
    Numerische Mathematik,
    Volume 12, Number 5, December 1968, pages 377-383.

  Parameters:

    Input/output, double D(N), the diagonal entries of the matrix.
    On output, the information in D has been overwritten.

    Input/output, double E(N), the subdiagonal entries of the
    matrix, in entries E(0:N-1).  On output, the information in
    E has been overwritten.

    Input/output, double Z(N).  On input, a vector.  On output,
    the value of Q' * Z, where Q is the matrix that diagonalizes the
    input symmetric tridiagonal matrix.
"
  (declare (type (array double-float (*)) z e d)
	   (type fixnum max-iterations n)
	   (type double-float prec))
  (assert (= n (length e) (length z)) nil "vectors not of same length")
  (prog ((s 0.0d0) (r 0.0d0) (p 0.0d0) (g 0.0d0) (f 0.0d0) (c 0.0d0)
	 (b 0.0d0) (mml 0) (m 0) (l 0) (k 0) (j 0) (ii 0) (i 0))
     (declare (type (double-float) s r p g f c b)
	      (type fixnum mml m l k j ii i))
     (if (= n 1) (go end_label))
     (setf (aref e (1- n)) 0d0)
     (%for (setf l 1) (<= l n) (incf l)
	   (setf j 0)
	   (loop
	     (%for (setf m l) (<= m n) (incf m)
		   (if (= m n) (return))
		   (if (<= (abs (aref e (1- m))) (* prec (+ (abs (aref d (1- m))) (abs (aref d m))))) (return)))
	     (setf p (aref d (1- l)))
	     (if (= m l) (return))
	     (if (>= j max-iterations) (error "IMTQLX - Fatal error! Iteration limit exceeded"))
	     (setf j (+ j 1)
		   g (/ (- (aref d l) p) (* 2 (aref e (1- l))))
		   r (sqrt (+ (* g g) 1d0))
		   g (+ (- (aref d (1- m)) p) (/ (aref e (1- l)) (+ g (float-sign g r))))
		   s 1d0
		   c 1d0
		   p 0d0
		   mml (- m l))
	     (%for (setf ii 1) (<= ii mml) (incf ii)
		   (setf i (- m ii)
			 f (* s (aref e (1- i)))
			 b (* c (aref e (1- i))))
		   (if (<= (abs g) (abs f))
		       (setf c (/ g f)
			     r (sqrt (+ (* c c) 1d0))
			     (aref e i) (* f r)
			     s (/ r)
			     c (* c s))
		       (setf s (/ f g)
			     r (sqrt (+ (* s s) 1d0))
			     (aref e i) (* g r)
			     c (/ r)
			     s (* s c)))
		   (setf g (- (aref d i) p)
			 r (+ (* (- (aref d (1- i)) g) s) (* 2 c b))
			 p (* s r)
			 (aref d i) (+ g p)
			 g (- (* c r) b)
			 f (aref z i)
			 (aref z i) (+ (* s (aref z (1- i))) (* c f))
			 (aref z (1- i)) (- (* c (aref z (1- i))) (* s f))))
	     (setf (aref d (1- l)) (- (aref d (1- l)) p)
		   (aref e (1- l)) g
		   (aref e (1- m)) 0d0)))
     (%for (setf ii 2) (<= ii n) (incf ii)
	   (setf i (- ii 1)
		 k i
		 p (aref d (1- i)))
	   (%for (setf j ii) (<= j n) (incf j)
		 (if (< (aref d (1- j)) p)
		     (setf k j
			   p (aref d (1- j)))))
	   (if (/= k i)
	       (setf (aref d (1- k)) (aref d (1- i))
		     (aref d (1- i)) p
		     p (aref z (1- i))
		     (aref z (1- i)) (aref z (1- k))
		     (aref z (1- k)) p)))
   end_label
     (return (values d z))))
