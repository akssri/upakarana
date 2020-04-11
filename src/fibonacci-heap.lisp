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

(in-package #:upakarana-fibonacci-heap)

;; Based on CLR, Chapter 19. CLR refers to 'key' as the values of the nodes (node.car) that are
;; to be compared. The term key (node.key) is used here in the context of key-value store, and
;; is meant for keeping track of vertices etc. in graph-algorithms.

(defstruct (node (:include dcons) (:conc-name n))
  ;; [dcar | drdc | dcdr]
  ;;         /        \
  ;;  [.|.| . ]   [.| . |.]
  (parent nil :type (or null node))
  (children nil :type (or null node))
  (degree 0 :type fixnum)
  (markp nil :type boolean)
  (index -1 :type fixnum))

(defmethod print-object ((nd node) stream)
  (print-unreadable-object (nd stream :type t)
    (format stream "car: ~A, degree: ~A, markp: ~A, rdc: ~A, cdr: ~A" (dcar nd) (ndegree nd) (nmarkp nd) (drdc nd) (dcdr nd))))

(defun node (se1 &optional (index -1))
  "(NODE se1 &optional [index -1]) => node
   se1: object held in the node
   index: auxillary pointer"
  (let ((nd (make-node :car se1 :index index)))
    (setf (drdc nd) nd (dcdr nd) nd)
    nd))

(defun dsplice! (rlst1 rlst2)
  ";; rlst1, rlst2 assumed to be d.l rings
   ;; (DSPLICE! |a - b - c - a| |x - y - z - x|)
   ;; => |a - b - c - x - y - z - a|"
  (if rlst2
      (let ((rend.1 (drdc rlst1))
	    (rend.2 (drdc rlst2)))
	(setf (dcdr rend.1) rlst2
	      (drdc rlst2) rend.1
	      (dcdr rend.2) rlst1
	      (drdc rlst1) rend.2)))
  rlst1)
;;
(defclass fibonacci-heap ()
  ((xnode :initform nil :reader xnode :documentation "extremum node")
   (size :initform 0 :reader size :documentation "number of elements in the heap")
   (order :initarg :order :reader order :initform #'< :documentation "ordering of the elements")
   (node-table :initform (make-extensible-vector) :reader node-table)))

(defmethod print-object ((fib fibonacci-heap) stream)
  (print-unreadable-object (fib stream :type t)
    (format stream "size: ~A" (size fib))))

;; MIN-KEY
(declaim (inline peek))
(defun peek (fib)
  "(PEEK fib) => NIL or (extreme-value, extremum-node-table-index)
  Return, if it exists, the extreme element and its corresponding node index in NODE-TABLE; O(1)."
  (when-let (min (xnode fib))
    (values (dcar min) (nindex min))))

;; INSERT
(defun push (obj fib &aux (node (node obj (length (node-table fib)))))
  "(PUSH obj fib) => fib
  INSERT. Insert object into heap; O(1)."
  (declare (type fibonacci-heap fib))
  (vector-push-extend node (node-table fib))
  (if-let ((root (xnode fib)))
    (progn
      (dsplice! node root)
      (if (funcall (order fib) obj (dcar root))
	  (setf (slot-value fib 'xnode) node)))
    (setf (slot-value fib 'xnode) node))
  (incf (slot-value fib 'size))
  fib)

;; EXTRACT-MIN
(defun pop (fib)
  "(pop fib) => extreme-value
  EXTRACT-MIN. This operation extricates the extremum node from the Fibonacci heap @arg{fib}; O(log n).
  The delayed work of consolidating trees in the root list occurs within this function (CLR 3rd ed)."
  (declare (type fibonacci-heap fib))
  (when-let ((prev-xnode (xnode fib)))
    (when-let (xnode.children (nchildren prev-xnode))
      (iter (for child on-dlist xnode.children until (drdc xnode.children)) (setf (nparent child) nil))
      (dsplice! xnode.children prev-xnode))
    (dpop (slot-value fib 'xnode)) ;; remove prev-xnode from heap
    (decf (slot-value fib 'size))
    (cond
      ((= 0 (size fib)) (setf (slot-value fib 'xnode) nil))
      ((eql (xnode fib) (drdc (xnode fib))) nil) ;;single tree
      (t (consolidate fib)))
    ;; remove pointers
    (setf (dcdr prev-xnode) nil (drdc prev-xnode) nil (ndegree prev-xnode) -1
	  (nchildren prev-xnode) nil (nparent prev-xnode) nil (nmarkp prev-xnode) nil)
    (values (dcar prev-xnode) (nindex prev-xnode))))

(defun consolidate (fib)
  (letv* ((root (xnode fib))
	  (order (slot-value fib 'order))
	  (degree-table (make-array (+ 2 (integer-length (size fib))) :initial-element nil)))
    (iter (for w on-dlist root until (drdc root))
	  (repeat (size fib))
	  (iter (for y in-vector degree-table with-index dd from (ndegree w)) (with x = w)
		(unless y
		  (setf (aref degree-table dd) x (ndegree x) dd)
		  (finish))
		;; exchange nodes if y < x
		(when (funcall order (dcar y) (dcar x)) (rotatef y x))
		(setf
		 ;; cut node y from root list
		 root (let ((root_y y)) (dpop root_y) root_y)
		 ;; make y a child of x
		 (nparent y) x (nmarkp y) nil (drdc y) y (dcdr y) y
		 (nchildren x) (dsplice! y (nchildren x))
		 ;; update root-degree table
		 (aref degree-table dd) nil)))
    ;;update min
    (iter (for x on-dlist root until (drdc root))
	  (with fmin = nil)
	  (when (or (null fmin) (funcall order (dcar x) (dcar fmin))) (setf fmin x))
	  (finally (setf (slot-value fib 'xnode) fmin)))
    fib))

;; DECREASE-KEY
(define-condition fibonacci-heap-decrement-infeasible (error)
  ((new :initarg :new)
   (index :initarg :index)))

(defun decrement (new index fib &aux (ord (order fib)))
  "(DECREMENT new index fib)
  DECREASE-KEY. Decrease the key of node @arg{index} to @arg{new}; O(1)."
  (declare (type fibonacci-heap fib))
  (let ((x (aref (node-table fib) index)))
    (declare (type node x))
    (assert (not (funcall ord (dcar x) new)) nil 'fibonacci-heap-decrement-infeasible :new new :index index)
    (setf (dcar x) new)
    (when (dcdr x)
      ;;cut node
      (when-let (y (nparent x))
	(when (funcall ord new (dcar y))
	  (cut x y fib) (ccut y fib)))
      ;;update min
      (when (funcall ord new (dcar (xnode fib)))
	(setf (slot-value fib 'xnode) x))))
  new)

;; DELETE NODE
(defun delete (index fib &optional deletep)
  "(DELETE index fib &optional deletep) => node-value, node-key
  delete the node associated with @arg{index} from @arg{fib}."
  (declare (type fibonacci-heap fib))
  (let ((x (aref (node-table fib) index)))
    (declare (type node x))
    (when (dcdr x)
      ;;cut node
      (when-let (y (nparent x))
	(cut x y fib) (ccut y fib))
      ;;move to min
      (setf (slot-value fib 'xnode) x)
      (upakarana-fibonacci-heap::pop fib)
      (when deletep (setf (aref (node-table fib) (nindex x)) nil)))
    (values (dcar x) (nindex x))))
;;
(defun cut (x xp fib)
  ;; remove x from the children of y
  (decf (ndegree xp))
  (setf (nchildren xp) (let ((child_x x)) (dpop child_x) (if (= 0 (ndegree xp)) nil child_x)))
  ;; add x to the root list of fib
  (setf (nparent x) nil (nmarkp x) nil (dcdr x) x (drdc x) x)
  (dsplice! x (xnode fib)))

(defun ccut (y fib)
  (when-let ((z (nparent y)))
    (if (nmarkp y)
	(progn (cut y z fib) (ccut z fib))
	(setf (nmarkp y) t))))
;;
