;; cloop is a polyhedral compiler for Common Lisp.
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
   (pivot-history :initarg :pivot-history)
   (basic-variables :initarg :basic-variables)))

(define-condition simplex-unbounded (error)
  ((unbounded-variables :initarg :unbounded-variables)
   (pivot-history :initarg :pivot-history)
   (basic-variables :initarg :basic-variables)))

(define-condition simplex-exceeded-max-iterations (error)
  ((count :initarg :count)
   (pivot-history :initarg :pivot-history)
   (basic-variables :initarg :basic-variables)))

;;
(defstruct csc-matrix
  (m 0 :type simplex-itype)
  (n 0 :type simplex-itype)
  (ptr (make-array 0 :element-type 'simplex-itype) :type (simple-array simplex-itype (*)))
  (idx (make-array 0 :element-type 'simplex-itype) :type (simple-array simplex-itype (*)))
  (val (make-array 0 :element-type 'simplex-dtype) :type (simple-array simplex-dtype (*))))

(defun dense-csc-gemm! (alpha A B col-B beta C)
  "compute alpha A^T B + beta C^T"
  (if (/= beta 1) (<- (_ii _jj) (aref C _ii _jj) (* beta (aref C _ii _jj))))
  (let ((ptr (csc-matrix-ptr B))
	(idx (csc-matrix-idx B))
	(val (csc-matrix-val B)))
    (loop :for b-jj :across col-B
       :do (loop :for ptr-kk :from (aref ptr b-jj) :below (aref ptr (1+ b-jj))
	      :do (let ((kk (aref idx ptr-kk))
			(s (* alpha (aref val ptr-kk))))
		    (<- (_ii) (aref c c-jj _ii) (+ (aref c c-jj _ii) (* s (aref A kk _ii))))))
       :counting t :into c-jj))
  C)

(defun gemm! (alpha A b beta c &optional transpose-A)
  "compute alpha A B + beta C"
  (if (/= beta 1) (<- (_ii _jj) (aref c _ii _jj) (* beta (aref c _ii _jj))))
  (if (not transpose-A)
      (loop :for ii :from 0 :below (array-dimension A 0)
	 :do (loop :for kk :from 0 :below (array-dimension A 1)
		:do (let ((s (* alpha (aref A ii kk))))
		      (<- (_jj) (aref c ii _jj) (+ (aref c ii _jj) (* s (aref b kk _jj)))))))
      (error "not implemented"))
  c)

(defun gemv! (alpha A b beta c &optional transpose-A)
  "compute A B + beta C"
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

;;
(defun parse-constraints (c A op b)
  (letv* ((row-basic (make-array (length b) :initial-element nil :fill-pointer t :adjustable t))
	  (n-slack 0) (n-artificial 0))
    ;;parse constraints, add artificial variables for phase-1
    (iter (for op-ii in-vector op with-index ii)
	  (ecase op-ii
	    ((:=) (push (list* ii (list :artificial (prog1 n-artificial (incf n-artificial)))
			      (if (<= 0 (aref b ii)) 1 -1))
		       A))
	    ((:<= :>=)
	     (let ((sign (ecase op-ii (:<= 1) (:>= -1))))
	       (push (list* ii (list :slack (prog1 n-slack (incf n-slack))) sign) A)
	       (when (> 0 (* sign (aref b ii)))
		 (push (list* ii (list :artificial (prog1 n-artificial (incf n-artificial))) (- sign)) A)))))
	  (setf (aref row-basic ii) (second (first A))))
    (values A row-basic n-slack n-artificial)))

(defun make-tableau (c A op b)
  (letv* ((A row-basic n-slack n-artificial (parse-constraints c A op b))
	  (m (length b)) (n (length c)))
    (flet ((index (idx &optional type) (+ idx (case type (:slack n) (:artificial (+ n n-slack)) (t 0)))))
      (letv* ((A (sort (remove-if #'(lambda (x) (= (cddr x) 0)) ;;remove zeros
				  (map 'vector #'(lambda (x)
						   (destructuring-bind (ii jj . dd) x
						       (if (listp jj)
							   (letv* (((type idx) jj))
							     (list* ii (index idx type) dd))
							   x)))
				       A)) ;; sort in reverse-lex
		       #'(lambda (x y) (if (/= (second x) (second y))
					   (< (second x) (second y))
					   (< (first x) (first y))))))
	      (n-total (+ n n-slack n-artificial))
	      (A-csc (make-csc-matrix :m m :n n-total ;;create CSC matrix
				      :ptr (let ((ptr (make-array (1+ n-total) :element-type 'simplex-itype)))
					     (iter (for jj from 0 below (1- (length ptr)))
						   (iter (for dat in-vector A from (aref ptr jj) with-index kk)
							 (letv* (((_i _j . _d) dat)) (when (/= jj _j) (finish)))
							 (finally (setf (aref ptr (1+ jj)) kk))))
					     ptr)
				      :idx (map '(simple-array simplex-itype (*)) #'first A)
				      :val (map '(simple-array simplex-dtype (*))
						#'(lambda (x) (coerce (cddr x) 'simplex-dtype)) A)))
	      (new-c (let ((new-c (make-array n-total :element-type 'simplex-dtype)))
		       (<- ((_ii 0 (length c))) (aref new-c _ii) (coerce (aref c _ii) 'simplex-dtype))))
	      (new-b (let ((new-b (make-array m :element-type 'simplex-dtype)))
		       (<- (_ii) (aref new-b _ii) (coerce (aref b _ii) 'simplex-dtype)))))
	;;
	(letv* ((Btinv (make-array (list m m) :element-type 'simplex-dtype))
		(col-basic (make-array (csc-matrix-n A-csc) :initial-element nil)))
	  ;;translate index, setup col-basic array
	  (iter (for rb in-vector row-basic with-index ii)
		(letv* (((type idx) rb)
			(new-idx (index idx type)))
		  (setf (aref row-basic ii) new-idx
			(aref col-basic new-idx) ii)))
	  ;; scale A-csc and new-b, setup Binv
	  (let ((ptr (csc-matrix-ptr A-csc))
		(idx (csc-matrix-idx A-csc))
		(val (csc-matrix-val A-csc)))
	    ;;scale A-csc
	    (iter (for ii in-vector idx with-index ii-ptr)
		  (when (< (aref new-b ii) 0)
		    (setf (aref val ii-ptr) (- (aref val ii-ptr)))))
	    ;;scale new-b
	    (iter (for ii from 0 below (length new-b))
		  (when (< (aref new-b ii) 0)
		    (setf (aref new-b ii) (- (aref new-b ii)))))
	    ;;set-up Binv
	    (iter (for ii from 0 below (length new-b))
		  (letv* ((var-ii (aref row-basic ii))
			  (nil (assert (= (1+ (aref ptr var-ii)) (aref ptr (1+ var-ii))) nil "non diagonal at ~a" ii)) ;;assert diagonal
			  (dd (aref val (aref ptr var-ii))))
		    (setf (aref Btinv ii ii) (/ dd)))))
	  (values Btinv new-c A-csc new-b row-basic col-basic n-slack n-artificial))))))

(defun pivot! (Btinv row Acol)
  (loop :for ii :from 0 :below (array-dimension Btinv 0)
     :do (when (/= row ii)
	   (let ((s (/ (aref Acol 0 ii) (aref Acol 0 row))))
	     (<- (_jj) (aref Btinv _jj ii) (- (aref Btinv _jj ii) (* s (aref Btinv _jj row)))))))
  (let ((s (/ (aref Acol 0 row))))
    (<- (_jj) (aref Btinv _jj row) (* s (aref Btinv _jj row))))
  Btinv)

(defun simplex (_c _A op _b &key (max-iterations 100))
  (letv* ((Btinv c A b row-basic col-basic n-slack n-artificial (make-tableau _c _A op _b))
	  (n (length c)) (m (length b))
	  ;;workspace
	  (non-basic (make-array n :element-type 'simplex-itype :fill-pointer 0))
	  (cb (make-array (list m 1) :element-type 'simplex-dtype))
	  (cb.Btinv (make-array (list m 1) :element-type 'simplex-dtype))
	  (cnb (make-array (list (- n m) 1) :element-type 'simplex-dtype))
	  ;;
	  (Btinv.Ajj (make-array (list 1 m) :element-type 'simplex-dtype))
	  (Binv.b (make-array (list m) :element-type 'simplex-dtype)))
    ;;warm start ಇಗಾಗಿ ಇದನ್ನು ಬಿಡಿಸ ಬೇಕು
    (labels ((compute-cost (c &optional (ncols n))
	       (setf (fill-pointer non-basic) 0)
	       (iter (for cb in-vector col-basic below ncols with-index ii)
		     (unless cb
		       (vector-push ii non-basic)
		       (setf (aref cnb rii 0) (aref c ii))
		       (counting t into rii)))
	       (iter (for rb in-vector row-basic with-index ii)
		     (setf (aref cb ii 0) (aref c rb)))
	       (gemm! 1 Btinv cb 0 cb.Btinv)
	       (values (dense-csc-gemm! -1 cb.Btinv A non-basic 1 cnb)
		       (loop :for ii :from 0 :below (length b)
			  :summing (* (aref b ii) (aref cb.Btinv ii 0)))))
	     (compute-column (jj)
	       (values (dense-csc-gemm! 1 Btinv A (vector jj) 0 Btinv.Ajj)
		       (gemv! 1 Btinv b 0 Binv.b t)))
	     (compute-b () (gemv! 1 Btinv b 0 Binv.b t))
	     (solve (c &optional (ncols n))
	       (iter (repeat max-iterations)
		     (letv* ((c.nb c-lp (compute-cost c ncols))
			     (amax-c (iter (for ii from 0 below (array-dimension c.nb 0))
					   (let ((ci (aref c.nb ii 0))) ;;bland's rule
					     (if (< ci 0) (finding ii maximizing (- ci))))))
			     (nil (unless amax-c (return c-lp)))
			     (amax-c (aref non-basic amax-c))
			     ;;(nil (print amax-c))
			     (A_j b (compute-column amax-c))
			     (row (iter (for ii from 0 below (array-dimension A_j 1))
					(if (< 0 (aref A_j 0 ii))
					    (finding ii minimizing (/ (aref b ii) (aref A_j 0 ii)))))))
		       (if (not row) (error 'simplex-unbounded :unbounded-variables (list amax-c)
					    :pivot-history btinv :basic-variables row-basic)
			   (progn
			     (pivot! Btinv row A_j)
			     (setf (aref col-basic (aref row-basic row)) nil
				   (aref row-basic row) amax-c
				   (aref col-basic amax-c) row))))
		     (finally (restart-case (error 'simplex-exceeded-max-iterations :count max-iterations
						   :pivot-history btinv :basic-variables row-basic)
				(continue (answer) (when answer (solve c ncols))))))))
      ;;phase-1
      (let ((c (make-array (length c) :element-type 'simplex-dtype)))
	(iter (for rv in-vector row-basic) (setf (aref c rv) (coerce 1 'simplex-dtype)))
	(unless (= 0 (solve c))
	  (error 'simplex-infeasible
		 :pivot-history (compute-b) :basic-variables row-basic)))
      ;;phase-2
      (let* ((opt (solve c (- n n-artificial)))
	     (x (compute-b))
	     (ret (make-array (- n n-artificial) :initial-element (coerce 0 'simplex-dtype) :element-type 'simplex-dtype)))
	(iter (for ri in-vector row-basic with-index ii) (setf (aref ret ri) (aref x ii)))
	(values opt ret)))))
;;
