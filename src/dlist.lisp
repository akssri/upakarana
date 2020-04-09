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

(in-package #:matlisp-dlist)
;; (setf *print-circle* t)

(defstruct (dcons (:conc-name d))
  "[<car> <rdc> <cdr>]"
  car rdc cdr)

(defun dcons (obj rdc cdr)
  "(DCONS obj rdc cdr)
   Creates a new dcons cell, and populates its slots such that the inversion property is preserved.

   That is, the following holds, when (rdc x), (cdr x) are also dcons cells.
   (cdr (rdc x)) => x
   (rdc (cdr x)) => x

   Achieving this requires the modification of the adjoining dcons cells.
   This function creates new adjoining dcons cells for maintaining the property.

   For a destructive version see DCONS!
"
  (let* ((ret (make-dcons :car obj :rdc nil :cdr nil))
	 (rdc (typecase rdc
		(dcons (make-dcons :car (dcar rdc) :rdc (drdc rdc) :cdr ret))
		(t rdc)))
	 (cdr (typecase cdr
		(dcons (make-dcons :car (dcar cdr) :rdc ret :cdr (dcdr cdr)))
		(t cdr))))
    (setf (drdc ret) rdc
	  (dcdr ret) cdr)
    ret))

;; iterate macros
(defmacro-clause (FOR var ON-DLIST dlst &optional IN-REVERSE reversep UNTIL end-dcons)
  "All unique elements on the dlist."
  (with-gensyms (%dlst %end %next %finishp)
    `(progn
       (with ,%dlst = ,dlst) (with ,%next = nil)
       ,@(if end-dcons `((with ,%end = ,end-dcons) (with ,%finishp = nil)))
       (for ,var initially ,%dlst then ,%next)
       (setf ,%next ,(if (not reversep) `(dcdr ,var) `(drdc ,var)))
       ,@(if end-dcons `((if ,%finishp (finish))))
       (typecase ,var
	 (dcons ,@(if end-dcons `((if (eql ,var ,%end) (setf ,%finishp t)))))
	 (t (finish))))))

(defmacro-clause (FOR var IN-DLIST dlst &optional IN-REVERSE reversep UNTIL end-dcons)
  (with-gensyms (%var*)
    `(progn
       (for ,%var* on-dlist ,dlst in-reverse ,reversep until ,end-dcons)
       (for ,var = (dcar ,%var*)))))
;;

(defun dlist (&rest objs &aux (ret (dcons (car objs) nil nil)))
  (do ((objs (cdr objs) (cdr objs))
       (lst ret (dcons! (car objs) lst nil)))
      ((null objs) ret)))

(defun dring (&rest objs &aux (ret (dcons (car objs) nil nil)))
  (setf (drdc ret) ret (dcdr ret) ret)
  (do ((objs (cdr objs) (cdr objs))
       (lst ret (dcons! (car objs) lst ret)))
      ((null objs) ret)))

(defun dlast (dlst &optional reversep)
  (if (not reversep)
      (iter (for %x on-dlist dlst)
	    (for %xp previous %x)
	    (finally (return %xp)))
      (iter (for %x on-dlist dlst in-reverse t)
	    (for %xp previous %x)
	    (finally (return %xp)))))

(defun dcons! (obj rdc cdr)
  (let ((ret (make-dcons :car obj :rdc rdc :cdr cdr)))
    (typecase rdc (dcons (setf (dcdr rdc) ret)))
    (typecase cdr (dcons (setf (drdc cdr) ret)))
    ret))

(defmacro dpush (obj dll &environment env)
  (multiple-value-bind (dummies vals new setter getter) (get-setf-expansion dll env)
    (when (cdr new) (error "can't expand this."))
    (with-gensyms (left ncon)
      (let ((new (car new)))
	`(let* (,@(zip dummies vals)
		(,new ,getter)
		(,ncon (dcons ,obj nil nil)))
	   (etypecase ,new
	     (dcons
	      (let ((,left (drdc ,new)))
		(setf (drdc ,ncon) ,left)
		(typecase ,left (dcons (setf (dcdr ,left) ,ncon)))
		(setf (dcdr ,ncon) ,new
		      (drdc ,new) ,ncon)))
	     (null))
	   (setf ,new ,ncon)
	   ,setter)))))

(defmacro dpop (dll &environment env)
  (multiple-value-bind (dummies vals new setter getter) (get-setf-expansion dll env)
    (when (cdr new) (error "can't expand this."))
    (with-gensyms (left right)
      (let ((new (car new)))
	`(let* (,@(zip dummies vals)
		(,new ,getter))
	   (when ,new
	     (let* ((,left (drdc ,new))
		    (,right (dcdr ,new)))
	       (prog1 (dcar ,new)
		 (typecase ,left (dcons (setf (dcdr ,left) ,right)))
		 (typecase ,right (dcons (setf (drdc ,right) ,left)))
		 (setf ,new ,right)
		 ,setter))))))))
