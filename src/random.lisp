
(in-package #:upakarana-random)

;; NOTE: all of the samplers are built using random-byte-kernel
(declaim (inline random-byte random-uniform))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun random-byte (arg)
    "Sample from the Uniform distribution on {0, arg-1}"
    (random (the unsigned-byte arg)))
  (defun uniform (&optional (type 'double-float))
    "Sample from the Uniform distribution on [0, 1)"
    (ecase type
      (single-float (scale-float (float (random-byte-kernel #.(expt 2 23)) 1.0) -23))
      (double-float (scale-float (float (random-byte-kernel #.(expt 2 52)) 1d0) -52)))))

;;
(defun pareto (a b)
  "Sample from the pareto distribution:
   p(x) dx = (a/b) / (x/b)^(a+1) dx     for x >= b

   Originally from cl-randist by Leonardo Varuzza et.al."
  (declare (type double-float a b))
  (let* ((x (- 1 (random-uniform-kernel)))
	 (z (expt x (/ -1d0 a))))
    (declare (type double-float x z))
    (* b z)))

;; exponential distribution
(declaim (ftype (function () double-float) exponential))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline exponential-function exponential-tail-sampler))
  (defun exponential-function (x) (exp (- x)))
  (defun exponential-tail-sampler (r0 f0 &optional (type 'double-float))
    (let* ((u1 (- 1 (random-uniform-kernel type))))
      (+ r0 (* -1 (log u1)))))
  (letv* ((points v (u.ziggurat:ziggurat-bisect #'exponential-function 15d0 :n-divisions (expt 2 8) :x_0-atol 1d-14))
	  (points (cons 0d0 (cdr points)))
	  (form (u.ziggurat:ziggurat-compile #'exponential-function points v
					     #'random-uniform-kernel #'random-byte-kernel #'exponential-tail-sampler
					     :symmetricp nil)))
    (setf (symbol-function 'exponential)
	  (funcall (compile nil `(lambda () ,form)))
	  (documentation 'exponential 'function)
  "Sample from the standard exponential distribution:
   p(x) dx = exp(-x) dx     for x >= 0")))

;; normal distribution
(declaim (ftype (function () double-float) normal))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline gaussian-function gaussian-tail-sampler))
  (defun gaussian-function (x) (exp (* -1/2 x x)))
  (defun gaussian-tail-sampler (r0 f0 &optional (type 'double-float))
    (let* ((u1 (random-uniform-kernel type))
	   (u2 (random-uniform-kernel type))
	   (x (* -1 (/ r0) (log u1)))
	   (y (- 1 (log u2))))
      (if (> (+ y y) (* x x)) (+ x r0))))
  (letv* ((points v (u.ziggurat:ziggurat-bisect #'gaussian-function 1d0 :n-divisions (expt 2 7)))
	  (points (cons 0d0 (cdr points)))
	  (form (u.ziggurat:ziggurat-compile #'gaussian-function (cons 0d0 (cdr points)) v
					     #'random-uniform-kernel #'random-byte-kernel #'gaussian-tail-sampler)))
    (setf (symbol-function 'normal)
	  (funcall (compile nil `(lambda () ,form)))
	  (documentation 'normal 'function)
  "Sample from the standard normal distribution:
   p(x) dx = (1//√2π) exp(-x^2/2) dx")))

;; gamma distribution
(declaim (ftype (function (double-float &optional double-float) double-float) gamma))
(defun gamma (a &optional (b 1d0))
  "Sample from the gamma distribution with shape a > 0, and scale b > 1 defined by,
   p(x) dx = {b^{a} \over \Gamma(a)} x^{a-1} e^{-bx} dx, x > 0,
   using the method of Marsaglia and Tsang [1].

   [1]. 'A Simple Method for generating gamma variables', ACM Transactions on Mathematical Software, Vol 26, No 3 (2000), p363-372.

   Implemented by J.D.Lamb@btinternet.com, minor modifications for GSL by Brian Gough.

   Originally from cl-randist by Leonardo Varuzza et.al."
  (declare (type (double-float 1d-256) a b))
  (if (< a 1d0)
      (* (gamma (+ 1d0 a) b) (expt (uniform) (/ a)))
      (let* ((x 0d0) (v 0d0) (u 0d0)
	     (d (- a (/ 3d0)))
 	     (c (/ (/ 3d0) (sqrt d))))
	(declare (double-float x v u d c))
	(tagbody
	 start
	   (setf x (normal)
		 v (+ 1d0 (* c x)))
	   ;;check validity
	   (when (<= v 0d0)
	     (go start))
	   (setf v (* v v v)
		 u (random-uniform))
	   ;;squeeze check
	   (when (< u (- 1d0 (* 0.0331d0 x x x x))) (go end))
	   ;;slow check
	   (when (< (log u) (+ (* 0.5d0 x x) (* d (+ 1 (- v) (the double-float (log v)))))) (go end))
	   ;;rinse-repeat
	   (go start)
	 end)
	(* b d v))))

;;beta distribution
(defun beta (a b)
  "Sample from the beta distribution,
   p(x) dx = (Gamma(a + b)/(Gamma(a) Gamma(b))) x^(a-1) (1-x)^(b-1) dx
   The method used here is the one described in Knuth.

   Originally from cl-randist by Leonardo Varuzza et.al."
  (declare (type double-float a b))
  (let ((x1 (gamma a))
	(x2 (gamma b)))
    (declare (double-float x1 x2))
    (/ x1 (+ x1 x2))))

;;chi-square
(defun chi-square (nu)
  "Sample from the chi square distribution:
   p(x) dx = (1/(2*Gamma(nu/2))) (x/2)^(nu/2 - 1) exp(-x/2) dx

   Originally from cl-randist by Leonardo Varuzza et.al."
  (declare (type double-float nu))
  (* 2d0 (gamma (/ nu 2d0))))

;;t distribution
(declaim (ftype (function (double-float) double-float) standard-t))
(defun standard-t (nu)
  "Sample from the standard t-distribution,
   p(x) dx = (Gamma((nu + 1)/2)/(sqrt(pi nu) Gamma(nu/2)) * (1 + (x^2)/nu)^-((nu + 1)/2) dx
   The method used here is the one described in Knuth.

   Originally from cl-randist by Leonardo Varuzza et.al."
  (declare (type double-float nu))
  (if (<= nu 2d0)
      (let ((y1 (normal))
	    (y2 (chi-square nu)))
	(/ y1 (sqrt (/ y2 nu))))
      (let ((y1 0d0) (y2 0d0) (Z 0d0))
	(declare (type double-float y1 y2 Z))
	(tagbody
	 start
	   (setf y1 (normal))
	   (setf y2 (* (/ (- (/ nu 2d0) 1d0)) (exponential)))
	   (setf z (/ (* y1 y1) (- nu 2d0)))
	   (when (or (< (- 1d0 z) 0)
		     (> (exp (- (- y2) z)) (- 1d0 z)))
	     (go start)))
	(/ y1 (sqrt (* (- 1d0 (/ 2d0 nu)) (- 1d0 z)))))))

;; dirichlet
(defun dirichlet (alpha)
  "The Dirichlet probability distribution of order K-1 is supported on the K-1 simplex
     \Delta : \sum_i=1,K \theta_i = 1,
   with the p.d.f,
     p(\theta_1,...,\theta_K) d\theta_1 ... d\theta_K = (1/Z) \prod_i=1,K \theta_i^{alpha_i - 1}

   The normalization factor Z can be expressed in terms of gamma functions:

      Z = {\prod_i=1,K \Gamma(\alpha_i)} / {\Gamma( \sum_i=1,K \alpha_i)}

   The K constants, \alpha_1,...,\alpha_K, must be positive. The K parameters,
   \theta_1,...,\theta_K are nonnegative and sum to 1.

   The random variates are generated by sampling K values from gamma
   distributions with parameters a=\alpha_i, b=1, and renormalizing.
   See A.M. Law, W.D. Kelton, Simulation Modeling and Analysis (1991).

   Originally by Gavin E. Crooks <gec@compbio.berkeley.edu> (2002)"
  (letv* ((alpha (coerce alpha '(simple-array double-float (*))))
	  (K (length alpha))
	  (ret (make-array K :element-type 'double-float)))
    (declare (type (simple-array double-float (*)) alpha))
    (let ((zz 0d0))
      (declare (type double-float zz))
      (<- (jj) (aref ret jj) (let ((sj (gamma (aref alpha jj))))
			       (incf zz sj)
			       sj))
      (<- (jj) (aref ret jj) (/ (aref ret jj) zz)))
    ret))
