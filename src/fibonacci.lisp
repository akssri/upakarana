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

(in-package #:upakarana-fibonacci)

;; Based on CLR, Chapter 19. CLR refers to 'key' as the values of the nodes (node.car) that are
;; to be compared. The term key (node.key) is used here in the context of key-value store, and
;; is meant for keeping track of vertices etc. in graph-algorithms.

(defun splice! (rlst1 rlst2)
  (if rlst2
      (let ((rend.1 (drdc rlst1))
	    (rend.2 (drdc rlst2)))
	(setf (dcdr rend.1) rlst2
	      (drdc rlst2) rend.1
	      (dcdr rend.2) rlst1
	      (drdc rlst1) rend.2)))
  rlst1)

(defstruct (node (:include dcons) (:conc-name n))
  (parent nil :type (or null node))
  (children nil :type (or null node))
  (degree 0 :type fixnum)
  (markp nil :type boolean)
  (key -1 :type fixnum))

(defmethod print-object ((nd node) stream)
  (print-unreadable-object (nd stream :type t)
    (format stream "key: ~A, degree: ~A, mark?: ~A, rdc: ~A, cdr: ~A" (dcar nd) (ndegree nd) (nmarkp nd) (drdc nd) (dcdr nd))))

(defun node (value &optional (key -1))
  "(NODE value &optional [key -1]) => node
   value: of the node
   key: auxillary pointer"
  (let ((nd (make-node :car value :key key)))
    (setf (drdc nd) nd (dcdr nd) nd)
    nd))
;;
(defclass fibonacci-heap ()
  ((xnode :initform nil :reader xnode :documentation "extremum node")
   (size :initform 0 :reader size :documentation "number of elements in the heap")
   (node-table :initform (make-hash-table :test 'eql) :reader node-table)
   (order :initarg :order :initform #'< :documentation "ordering of the elements")))
(defmethod print-object ((fib fibonacci-heap) stream)
  (print-unreadable-object (fib stream :type t)
    (format stream "size: ~A" (size fib))))

(defun fibonacci-heap (&optional (order #'<))
  (make-instance 'fibonacci-heap :order order))

(declaim (inline min-value node-existsp node-value))
(defun min-value (fib) (when-let (min (xnode fib)) (values (dcar min) (nkey min))))
(defun node-existsp (key fib)
  ;; assumptions - when node.dcdr is null, it is *not* an active part of the heap"
  (letv* ((node exists-p (gethash key (node-table fib))))
    (and exists-p (dcdr node) t)))
(defun node-value (key fib)  
  (letv* ((node exists-p (gethash key (node-table fib))))
    (when exists-p (values (dcar node) (nkey node)))))
(defun (setf node-value) (new-value key fib)
  (letv* ((x exists-p (gethash key (node-table fib)))
	  (order (slot-value fib 'order)))
    (if exists-p
	(if (dcdr x)
	    (if (funcall order new-value (dcar x))
		(decrease-key new-value x fib)
		(progn
		  (delete-key x fib t)
		  (insert new-value fib key)))
	    (progn
	      (remhash key (node-table fib))
	      (insert new-value fib key)))
	(insert new-value fib)))
  new-value)
;;
(defun insert (obj fib &optional key &aux (node (node obj (or key (size fib)))))
  "(INSERT obj fib) => fib
  Insert object into heap"
  (assert (not (node-existsp (nkey node) fib)) nil "node already exists with key ~a" (nkey node))
  (setf (gethash (nkey node) (node-table fib)) node)
  (if-let ((root (xnode fib)))
    (progn
      (splice! node root)
      (if (funcall (slot-value fib 'order) obj (dcar root))
	  (setf (slot-value fib 'xnode) node)))
    (setf (slot-value fib 'xnode) node))
  (incf (slot-value fib 'size))
  fib)
;;
(defun extract-min (fib)
  "(EXTRACT-MIN fib) => extreme-value
  This operation extricates the minimum node from the Fibonacci heap @arg{fib}. It's also where the delayed work of consolidating trees in the root list finally occurs (CLR 3rd ed)."
  (when-let ((prev-xnode (xnode fib)))
    (when-let (xnode.children (nchildren prev-xnode))
      (iter (for child on-dlist xnode.children until (drdc xnode.children)) (setf (nparent child) nil))
      (splice! xnode.children prev-xnode))
    (dpop (slot-value fib 'xnode)) ;; remove prev-xnode from heap
    (decf (slot-value fib 'size))
    (cond
      ((= 0 (size fib)) (setf (slot-value fib 'xnode) nil))
      ((eql (xnode fib) (drdc (xnode fib))) nil) ;;single tree
      (t (consolidate fib)))
    ;; remove pointers
    (setf (dcdr prev-xnode) nil (drdc prev-xnode) nil (ndegree prev-xnode) -1
	  (nchildren prev-xnode) nil (nparent prev-xnode) nil (nmarkp prev-xnode) nil)
    (values (dcar prev-xnode) (nkey prev-xnode))))

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
		 (nchildren x) (splice! y (nchildren x))
		 ;; update root-degree table
		 (aref degree-table dd) nil)))
    ;;update min
    (iter (for x on-dlist root until (drdc root))
	  (with fmin = nil)
	  (when (or (null fmin) (funcall order (dcar x) (dcar fmin))) (setf fmin x))
	  (finally (setf (slot-value fib 'xnode) fmin)))
    fib))
;;
(defun decrease-value (new-value node-or-key fib &aux (order (slot-value fib 'order)))
  "(DECREASE-KEY new-value node-or-key fib)
  decrease the heap-value of node @arg{node-or-key} to @arg{new-value}."
  (let ((x (etypecase node-or-key (node node-or-key) (fixnum (gethash node-or-key (node-table fib))))))
    (declare (type node x))
    (assert (not (funcall (dcar x) new-value)) nil "new key is greater than the current.")
    (setf (dcar x) new-value)
    (when (dcdr x)
      ;;cut node
      (when-let (y (nparent x))
	(when (funcall order new-value (dcar y))	  
	  (cut x y fib) (ccut y fib)))
      ;;update min
      (when (funcall order new-value (dcar (xnode fib)))
	(setf (slot-value fib 'xnode) x))))
  new-value)

(defun delete-key (node-or-key fib &optional deletep)
  "(DELETE-key node-or-key fib &optional deletep) => node-value, node-key
  delete the node associated with @arg{node-or-key} from @arg{fib}."
  (let ((x (etypecase node-or-key (node node-or-key) (fixnum (gethash node-or-key (node-table fib))))))
    (declare (type node x))    
    (when (dcdr x)
      ;;cut node
      (when-let (y (nparent x))
	(cut x y fib) (ccut y fib))
      ;;move to min
      (setf (slot-value fib 'xnode) x)
      (extract-min fib)
      (when deletep (remhash (nkey x) (node-table fib))))
    (values (dcar x) (nkey x))))
;;
(defun cut (x xp fib)
  ;; remove x from the children of y
  (decf (ndegree xp))
  (setf (nchildren xp) (let ((child_x x)) (dpop child_x) (if (= 0 (ndegree xp)) nil child_x)))
  ;; add x to the root list of fib  
  (setf (nparent x) nil (nmarkp x) nil (dcdr x) x (drdc x) x)
  (splice! x (xnode fib)))

(defun ccut (y fib)
  (when-let ((z (nparent y)))
    (if (nmarkp y)
	(progn (cut y z fib) (ccut z fib))
	(setf (nmarkp y) t))))
