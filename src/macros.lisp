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

(defmacro gethash! (key table default)
  (once-only (key table)
    (with-gensyms (value existsp)
      `(letv* ((,value ,existsp (gethash ,key ,table)))
	 (if ,existsp (values ,value t) (setf (gethash ,key ,table) ,default))))))

(defmacro assoc! (item alist &key key test test-not default &environment env)
  (multiple-value-bind (dummies vals new setter getter) (get-setf-expansion alist env)
    (when (cdr new) (error "can't expand this."))
    (setf new (car new))
    (once-only (item)
      (with-gensyms (cell)
	`(let* (,@(zip dummies vals)
		(,new ,getter))
	   (if-let ((,cell (assoc ,item ,new ,@(if key `(:key ,key)) ,@(if test `(:test ,test)) ,@(if test-not `(:test ,test-not)))))
	     ,cell
	     (let ((,cell (cons ,item ,default)))
	       (setf ,new (list* ,cell ,new))
	       ,setter
	       ,cell)))))))

(defmacro getf! (place indicator &optional default &environment env)
  (multiple-value-bind (dummies vals new setter getter) (get-setf-expansion place env)
    (when (cdr new) (error "can't expand this."))
    (setf new (car new))
    (once-only (indicator)
      (with-gensyms (ret dummy)
	`(let* (,@(zip dummies vals)
		(,new ,getter))
	   (let ((,ret (getf ,new ,indicator ',dummy)))
	     (if (not (eql ,ret ',dummy))
		 ,ret
		 (let ((,ret ,default))
		   (setf ,new (list* ,indicator ,ret ,new))
		   ,setter
		   ,ret))))))))

(defmacro accum! (value place &environment env)
  (multiple-value-bind (dummies vals new setter getter) (get-setf-expansion place env)
    (when (cdr new) (error "can't expand this."))
    (setf new (car new))
    (once-only (value)
      (with-gensyms (cell)
	`(let* (,@(zip dummies vals)
		(,new ,getter)
		  (,cell (list ,value)))
	   (if ,new
	       (progn
		 (rplacd (car ,new) ,cell)
		 (rplaca ,new ,cell)
		 ,new)
	       (progn
		 (setf ,new (cons ,cell ,cell))
		 ,setter)))))))
;;
(defmacro values-n (n &rest values)
  (once-only (n)
    (labels ((return-tree (values returns)
	       (with-gensyms (ret)
		 `((let ((,ret ,(car values)))
		     ,(let ((returns+ (cons ret returns)))
			(recursive-append
			 (when (cdr values)
			   `(if (> ,n ,(length returns+)) ,@(return-tree (cdr values) returns+)))
			 `(values ,@(reverse returns+)))))))))
      `(when (> ,n 0) ,@(return-tree values nil)))))

;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun simplify-list (lst dot-marker)
    (if (atom lst) lst
	(iter (for lst-head on lst)
	      (collect (simplify-list (car lst-head) dot-marker))
	      (typecase (cdr lst-head)
		((not list)
		 (collect dot-marker)
		 (collect (cdr lst-head)))))))

  (defun dottify-list (lst dot-marker)
    (if (atom lst) lst
	(labels ((dottify-worker (lst accum-head)
		   (cond
		     ((null lst))
		     ((eql (car lst) dot-marker)
		      (assert (not (cddr lst)) nil "malformed list. cannot translate.")
		      (setf (cdr accum-head) (cadr lst)))
		     (t (dottify-worker (cdr lst) (setf (cdr accum-head) (cons (car lst) nil)))))))
	  (let ((ret-head (cons nil nil)))
	    (dottify-worker lst ret-head)
	    (cdr ret-head))))))

(defmacro letv* (bindings &body body &aux (dot-marker (gensym "dot")))
  "
  This macro extends the syntax of let* to handle multiple values and destructuring bind,
  it also handles type declarations. The declarations list @arg{vars} is similar to that in let:
  look at the below examples.

  Examples:
  @lisp
  > (macroexpand-1 `(letv* ((x 2 :type fixnum)
			    ((a &optional (c 2)) b (values (list 1) 3) :type (fixnum &optional (t)) t))
		      t))
  => (LET ((X 2))
	   (DECLARE (TYPE FIXNUM X))
       (MULTIPLE-VALUE-BIND (#:G1120 B) (VALUES (LIST 1) 3)
	 (DECLARE (TYPE T B))
	 (DESTRUCTURING-BIND (A &OPTIONAL (C 2)) #:G1120
	   (DECLARE (TYPE FIXNUM A)
		    (TYPE T C))
	   (LOCALLY T))))
  @end lisp
  "
  (labels ((declare-types (vars types-alist)
	     (let ((decls (remove-if #'null (mapcar #'(lambda (s)
							(let ((spec (assoc s types-alist)))
							  (when spec
							    (if (second spec)
								`(type ,(second spec) ,s)
								`(ignore ,s)))))
						    vars))))
	       (when decls `((declare ,@decls)))))
	   (parse-binding (bind)
	     (let* ((type-position (position :type bind :from-end t))
		    (type-simp (if type-position (simplify-list (subseq bind (1+ type-position)) dot-marker)))
		    (bind (if type-position (subseq bind 0 type-position) bind))
		    (var-simp (simplify-list (butlast bind) dot-marker))
		    (expr (car (last bind))))
	       (assert (< 0 (length var-simp)) nil "malformed binding ~a" bind)
	       (if (equal var-simp '(nil)) ;; matches the expression block: (nil expr)
		   (progn (assert (null type-position) nil "given :type keyword, but deduced expression block.")
			  `((progn ,expr)))
		   (let* ((types-alist (maptree-if #'(lambda (x) (declare (ignore x)) t)
						   #'(lambda (x) (if (atom (car x))
								     (unless (member (car x) (cons dot-marker cl:lambda-list-keywords)) (list x))
								     (values x #'mapcan)))
						   (ziptree var-simp type-simp)))
			  (var-expand (mapcar #'(lambda (x) (if (atom x) (list x)
								(with-gensyms (g)
								  (list g `(destructuring-bind ,(dottify-list x dot-marker) ,g
									     ,@(declare-types (flatten x) types-alist))))))
					      var-simp)))
		     (list*
		      (recursive-append
		       (if (< 1 (length var-expand))
			   `(multiple-value-bind (,@(mapcar #'car var-expand)) ,expr)
			   `(let ((,(caar var-expand) ,expr))))
		       (car (declare-types (mapcar #'car var-expand) types-alist)))
		      (remove-if #'null (mapcar #'cadr var-expand))))))))
    (apply #'recursive-append
	   (append (mapcan #'parse-binding bindings)
		   `((locally ,@body))))))

;;
(flet ((cart-case-macrofunction (vars cases append &key (test 'eql) (list-is-orp t))
	 (let ((decl (mapcar #'(lambda (x) (list (gensym) x)) vars)))
	   `(let (,@decl)
	      (cond ,@(mapcar #'(lambda (clause)
				  `((and ,@(mapcar #'(lambda (x)
						       (letv* (((var case) x))
							 (if list-is-orp
							     `(or ,@(mapcar #'(lambda (u) `(,test ,var (quote ,u))) (ensure-list case)))
							     `(,test ,var (quote ,case)))))
						   (remove t (zip (mapcar #'car decl) (first clause)) :key #'second)))
				    ,@(cdr clause)))
			      cases)
		    ,@append)))))
  (defmacro cart-case ((&rest vars) &body cases)
    (cart-case-macrofunction vars cases nil))
  (defmacro cart-ecase ((&rest vars) &body cases)
    (cart-case-macrofunction vars cases `((t (error "cart-ecase: Case failure.")))))
  (defmacro cart-typecase ((&rest vars) &body cases)
    (cart-case-macrofunction vars cases nil :test 'typep :list-is-orp nil))
  (defmacro cart-etypecase ((&rest vars) &body cases)
    (cart-case-macrofunction vars cases `((t (error "cart-etypecase: Case failure."))) :test 'typep :list-is-orp nil)))

(defmacro with-memoization ((&optional (memo-table `(make-hash-table :test 'equal))) &body body)
  (with-gensyms (table)
    (labels ((transformer (x)
	       (destructuring-bind (define &rest body) x
		 (ecase define
		   ((cl:defun cl:defmethod #+closer-mop 'closer-mop:defmethod)
		    (destructuring-bind (name lambda-list &body body &aux (id (gensym "memo-"))) body
			`(,define ,name (,@lambda-list)
			   (values-list
			    (gethash! (list ',id ,@(mapcar (compose #'first #'ensure-list)
							   (set-difference
							    (take-while #'(lambda (x) (not (eql x '&aux))) lambda-list)
							    cl:lambda-list-keywords)))
				      ,table (multiple-value-list (locally ,@body)))))))
		   ((cl:labels cl:flet)
		    (destructuring-bind (definitions &body body) body
		      `(,define (,@(mapcar #'(lambda (x) (cdr (transformer `(cl:defun ,@x)))) definitions))
			   ,@body)))))))
      `(let ((,table ,memo-table)) ,@(mapcar #'transformer body)))))

;; threading macros ala Clojure
(defmacro .> (input &rest forms)
  (if (not forms) input
      (let ((form0 (first forms)))
	(etypecase form0
	  (cons `(.> (,(car form0) ,input ,@(cdr form0)) ,@(cdr forms)))
	  (symbol `(.> (slot-value ,input ',form0) ,@(cdr forms)))))))
