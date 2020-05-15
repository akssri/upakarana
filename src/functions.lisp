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

(in-package #:upakarana)

(defun make-extensible-vector (&key (element-type t))
  (make-array 0 :adjustable t :fill-pointer t :element-type element-type))

(defun slot-values (obj slots)
  (values-list (mapcar #'(lambda (s) (slot-value obj s)) slots)))
(define-compiler-macro slot-values (&whole form obj slots)
  (if (and (listp slots) (eql (car slots) 'quote) (null (cddr slots)))
      (once-only (obj)
	`(values ,@(mapcar #'(lambda (x) `(slot-value ,obj ',x)) (second slots))))
      form))

(defun maptree-if (predicate transformer tree)
  "(MAPTREE-IF predicate transformer tree)

  Returns a new tree by recursively calling @arg{transformer} on sub-trees which satisfy the @arg{predicate}.
  @arg{predicate} : tree -> boolean
  @arg{transformer}: tree -> (or tree atom) *control
  If the transformer returns a @arg{control} function, then the tree returned by
  the transformer is replaced in-turn by the result of:
  > (funcall @arg{control} #'(lambda (x) (maptree-if @arg{predicate} @arg{transformer} x)) transformed-tree)
  , otherwise it is left as it is.

  Example:
  @lisp
  > (maptree-if #'(λ (x) (and (consp x) (eq (car x) 'ping)))
		#'(λ (x) `(pong ,@(cdr x)))
		'(progn (ping (ping (ping 1)))))
  >= (PROGN (PONG (PING (PING 1))))
  > (maptree-if #'(λ (x) (and (consp x) (eq (car x) 'ping)))
		#'(λ (x) (values `(pong ,@(cdr x)) #'mapcar))
		'(progn (ping (ping (ping 1)))))
  >= (PROGN (PONG (PONG (PONG 1))))
  @end lisp
  "
  (multiple-value-bind (t-tree control) (if (funcall predicate tree)
					    (funcall transformer tree)
					    (values tree #'mapcar))
    (if (and (consp t-tree) control)
	(funcall (etypecase control (function control) ((eql t) #'mapcar)) #'(lambda (x) (maptree-if predicate transformer x)) t-tree)
	t-tree)))

(defun maptree (transformer tree)
  (multiple-value-bind (t-tree control) (funcall transformer tree)
    (if (and (consp t-tree) control)
	(funcall (etypecase control (function control) ((eql t) #'mapcar)) #'(lambda (x) (maptree transformer x)) t-tree)
	t-tree)))

(defun cartesian-product (list &rest more-lists)
  "(CARTESIAN-PRODUCT list &rest more-lists) => list1 x list2 x list3 ...

   Example:
   > (cartesian-product '(0 1) '(0 1)  '(0 1))
   ((0 0 0) (1 0 0) (0 1 0) (1 1 0) (0 0 1) (1 0 1) (0 1 1) (1 1 1))
"
  (if more-lists
      (mapcan #'(lambda (y) (mapcar #'(lambda (x) (cons x y)) list)) (apply #'cartesian-product more-lists))
      (mapcar #'list list)))

(defun mapcart (function list &rest more-lists)
  "(MAPCART list &rest more-lists) => (MAPCAR function (CARTESIAN-PRODUCT list *more-lists)"
  (mapcar (lambda (args) (apply function args)) (apply #'cartesian-product list more-lists)))

(defun take-while (pred lst)
  (loop :for x* :on lst
     :while (funcall pred (car x*))
     :collect (car x*) :into ret
     :finally (return (values ret x*))))

#+nil
(defun pair (list &optional (n 2))
  "(PAIR list [n 2])
  Groups elements in @arg{list} by @arg{n}.

  Example:
  @lisp
  > (pair '(2 3 4 5) 2)
  => ((2 3) (4 5))
  @end lisp
  "
  (loop :for x :on list :by #'(lambda (x) (nthcdr n x))
     :collect (subseq x 0 n)))

(defun zip (&rest args)
  "
  (ZIP *args)
  Zips the elements of @arg{args}.

  Example:
  @lisp
  > (zip '(2 3 4) '(a b c) '(j h c s))
  => ((2 A J) (3 B H) (4 C C))
  @end lisp
  "
  (if args (apply #'map 'list #'list args)))

(defun unzip (list)
  "
  (UNZIP list)
  UnZips the elements of @arg{args}.

  Example:
  @lisp
  > (unzip ((2 A J) (3 B H) (4 C C)))
  => ((2 3 4) (a b c) (j h c))
  @end lisp
  "
  (mapcar #'(lambda (n) (mapcar #'(lambda (x) (elt x n)) list))
	  (iter (for i from 0 below (length (first list))) (collect i))))

(defun ziptree (tree &rest more-trees)
  (if (atom tree)
      (cons tree more-trees)
      (apply #'mapcar (list* #'ziptree tree more-trees))))

(defun recursive-append (&rest lsts)
  "
  Appends lists in a nested manner, mostly used to bring in the charm of
  non-lispy languages into macros.

  Basically does
  @lisp
  (reduce
    #'(lambda (x y)
	(if (null x)
	  (if (typep (car y) 'symbol) y (car y))
	  (append x (if (null y) nil
			(if (typep (car y) 'symbol) `(,y) y)))))
    lsts :from-end t)
  @end lisp

  Examples:
  @lisp
  > (recursive-append
      '(let ((x 1)))
      '(+ x 2))
  => (LET ((X 1))
       (+ X 2))

  > (recursive-append
      '(let ((x 1)))
      '((let ((y 2))
	  (setq y 3))
	(let ((z 2))
	  z)))
  => (LET ((X 1))
       (LET ((Y 2))
	 (SETQ Y 3))
       (LET ((Z 2))
	 Z))

  > (recursive-append
      nil
      '((let ((x 1)) x)
	(progn (+ 1 2))))
  => (LET ((X 1))
       X)

  > (recursive-append nil '(let ((x 1)) x))
  => (LET ((X 1))
       X)
  @end lisp
  "
  (reduce #'(lambda (x y)
	      (if (null x)
		  (typecase (car y) (symbol y) (t (car y)))
		  (append x (and y (typecase (car y) (symbol (list y)) (t y))))))
	  lsts :from-end t))

(declaim (inline binary-search))
(defun binary-search (a x &key (lo 0) (hi (length a)) (order #'<))
 "(BINARY-SEARCH a x &key [lo 0] [hi (length a)] [order #'<]) => idx
  finds the smallest @arg{idx} \in [@arg{lo}, @arg{hi}) st., (not (order a[idx] x)) is satisfied; else returns @arg{hi}.
  assumptions: a is sorted under @arg{order}

  > (binary-search #(0 1 1 1) 1 :order #'<)  ;; left:  a[i-1] < x <= a[i]
  1
  > (binary-search #(0 1 1 1) 1 :order #'<=) ;; right: a[i-1] <= x < a[i]
  4"
  (declare (type (integer 0 #.array-dimension-limit) lo hi)
	   (type vector a))
  (if (not (funcall order (aref a lo) x)) lo
      (loop :while (> (- hi lo) 1) :do
	(let ((mid (floor (+ lo hi) 2)))
	  (if (funcall order (aref a mid) x)
	      (setf lo mid)
	      (setf hi mid)))
	    :finally (return hi))))

(declaim (inline sort-index))
(defun sort-index (seq predicate &key key)
  "(SORT-INDEX seq predicate &key key) => sorted-seq sort-index

  Similar to CL:SORT, except that the lisp-vector is sorted in-place (qsort), and the index of the sorted elements (permutation action) is returned.

  > (sort-index #(2 3 1 4 5) #'<)
  #(1 2 3 4 5)
  #(2 0 1 3 4)"
  (declare (type vector seq))
  (let* ((key (or key #'identity))
	 (len (length seq))
	 (perm (make-array len :element-type 'fixnum) )
	 (jobs (make-array len :adjustable t :fill-pointer 0)))
    (declare (type fixnum len)
	     (type (simple-array fixnum (*)) perm))
    (loop :for i :of-type fixnum :from 0 :below (length perm) :do (setf (aref perm i) i))
    (loop
       :for bounds := (cons 0 len) :then (unless (zerop (length jobs)) (vector-pop jobs))
       :until (null bounds)
       :do (let* ((below-idx (car bounds))
		  (above-idx (cdr bounds))
		  (piv (+ below-idx (floor (- above-idx below-idx) 2))))
	     (declare (type fixnum below-idx above-idx piv))
	     (loop
		:with ele := (funcall key (aref seq piv))
		:with lbound :of-type fixnum := below-idx
		:with ubound :of-type fixnum := (1- above-idx)
		:until (progn
			 (loop :for i :of-type fixnum :from lbound :to piv
			    :until (or (= i piv) (funcall predicate ele (funcall key (aref seq i))))
			    :finally (setq lbound i))
			 (loop :for i :of-type fixnum :downfrom ubound :to piv
			    :until (or (= i piv) (funcall predicate (funcall key (aref seq i)) ele))
			    :finally (setq ubound i))
			 (cond
			   ((= ubound lbound piv)
			    (when (> (- piv below-idx) 1)
			      (vector-push-extend (cons below-idx piv) jobs))
			    (when (> (- above-idx (1+ piv)) 1)
			      (vector-push-extend (cons (1+ piv) above-idx) jobs))
			    t)
			   ((< lbound piv ubound)
			    (rotatef (aref seq lbound) (aref seq ubound))
			    (rotatef (aref perm lbound) (aref perm ubound))
			    (incf lbound) (decf ubound)
			    nil)
			   ((= lbound piv)
			    (rotatef (aref seq piv) (aref seq (1+ piv)))
			    (rotatef (aref perm piv) (aref perm (1+ piv)))
			    (unless (= ubound (1+ piv))
			      (rotatef (aref seq piv) (aref seq ubound))
			      (rotatef (aref perm piv) (aref perm ubound)))
			    (incf piv) (incf lbound)
			    nil)
			   ((= ubound piv)
			    (rotatef (aref seq (1- piv)) (aref seq piv))
			    (rotatef (aref perm (1- piv)) (aref perm piv))
			    (unless (= lbound (1- piv))
			      (rotatef (aref seq lbound) (aref seq piv))
			      (rotatef (aref perm lbound) (aref perm piv)))
			    (decf piv) (decf ubound)
			    nil)))))
       :finally (return (values seq perm)))))

(defun topological-sort (seq porder &key (key #'identity))
  ;;(topological-sort '(fixnum integer double-float float (integer 0 10) real single-float) #'subtypep)
  ;;(sort '(fixnum integer double-float float (integer 0 10) real single-float) #'subtypep)
  (let* ((seq (coerce seq 'vector))
	 (adj (map 'vector (lambda (x) (cons 0 nil)) seq))
	 (stack nil))
    ;;create graph [*(n-parents . children)]
    (flet ((pred (a b) (funcall porder (funcall key a) (funcall key b))))
      (iter (for e.i in-vector seq with-index i)
	    (iter (for e.j in-vector seq with-index j)
		  (when (and (/= i j) (pred e.i e.j))
		    (push i (cdr (aref adj j)))
		    (incf (car (aref adj i)))))
	    (if (= 0 (car (aref adj i))) (push i stack))))
    ;;Kahn's algorithm
    (let* ((last-stack (last stack))
	   (ordering nil))
      (do* ((si* stack (cdr si*))
	    (si (car si*) (car si*)))
	   ((null si*))
	(push (aref seq si) ordering)
	(iter (for cj in (cdr (aref adj si)))
	      (when (= 0 (decf (car (aref adj cj))))
		(setf (cdr last-stack) (cons cj nil)
		      last-stack (cdr last-stack)))))
      ordering)))
