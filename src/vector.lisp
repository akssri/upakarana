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

(in-package #:upakarana-vector)

(declaim (inline map!))
(defun map! (func vec)
  (declare (type vector))
  (loop
     :for i :of-type fixnum :from 0 :below (length vec)
     :do (setf (aref vec i) (funcall func (aref vec i)))
     :finally (return vec)))

(declaim (inline foldl))
(defun foldl (func vec &optional accump!)
  (declare (type vector))
  (if accump!
      (loop
	 :for i :of-type fixnum :from 0 :below (length vec)
	 :for ret = (aref vec 0) :then (funcall func (aref vec i) ret)
	 :do (setf (aref vec i) ret)
	 :finally (return (values ret vec)))
      (loop
	 :for i :of-type fixnum :from 0 :below (length vec)
	 :for ret = (aref vec 0) :then (funcall func ret (aref vec i))
	 :finally (return ret))))

(declaim (inline foldr))
(defun foldr (func vec &optional accump!)
  (declare (type vector))
  (if accump!
      (loop
	 :for i :of-type fixnum :downfrom (1- (length vec)) :to 0
	 :for ret = (aref vec (1- (length vec))) :then (funcall func (aref vec i) ret)
	 :do (setf (aref vec i) ret)
	 :finally (return (values ret vec)))
      (loop
	 :for i :of-type fixnum :downfrom (1- (length vec)) :to 0
	 :for ret = (aref vec (1- (length vec))) :then (funcall func (aref vec i) ret)
	 :finally (return ret))))

(declaim (inline extreme))
(defun extreme (vec &optional (order #'<))
  (declare (type vector vec))
  (loop :for ele :across vec
     :for idx :of-type fixnum = 0 :then (+ idx 1)
     :with max = (aref vec 0)
     :with max-idx :of-type fixnum = 0
     :do (when (funcall order ele max)
	   (setf max ele
		 max-idx idx))
     :finally (return (values max max-idx))))

(declaim (inline copy!))
(defun copy! (n in of/in out of/out &key key lock)
  (declare (type fixnum n of/in of/out)
	   (type simple-array in)
	   (type simple-array out))
  (let ((key (or key #'row-major-aref))
	(lock (or lock #'(setf row-major-aref))))
    (declare (ignorable key lock))
    (loop :for i :of-type fixnum :from 0 :below n
       :do (funcall lock (funcall key in (the fixnum (+ of/in i))) out (the fixnum (+ of/out i))))
    out))

(defmacro <- (indices place &rest body)
  (letv* (((op arr &rest loop-indices) place))
    (assert (member op '(aref)))
    (alexandria:once-only (arr)
      `(progn
	 ,(apply #'recursive-append
		 (append (mapcar #'(lambda (idx)
				     (letv* (((var &optional start end) (alexandria:ensure-list idx)))
				       (let ((loop-dimension (position var loop-indices)))
					 `(loop :for ,var :from ,(or start 0) :below ,(or end `(array-dimension ,arr ,loop-dimension)) :do))))
				 indices)
			 `((setf (,op ,arr ,@loop-indices) (progn ,@body)))))
	 ,arr))))
