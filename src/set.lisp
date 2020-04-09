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

(in-package #:upakarana-hash-set)

(defun set-member (element table)
  (nth-value 1 (gethash element table)))

(defun add! (table element)  
  (gethash! element table t)
  table)

(defun remove! (table element)
  (remhash element table)
  table)

(defmacro iter-hset ((k s) &body body)
  (alexandria:once-only (s)
    `(etypecase ,s
       (list (iter (for ,k in ,s) (progn ,@body)))
       (hash-table (iter (for (,k ,(gensym "v")) in-hashtable ,s) (progn ,@body)))
       (vector (iter (for ,k in-vector ,s) (progn ,@body)))
       (t (let ((,k ,s)) ,@body)))))
;;
(defun set (sequence-or-element &optional (test 'eql) &aux (table (make-hash-table :test test)))
  (iter-hset (k sequence-or-element) (setf (gethash k table) t))
  table)

(defun set->list (hset)
  (if hset (iter (for (k v) in-hashtable hset) (collect k))))

(defun ensure-set (tbl &optional (test 'eql))
  (etypecase tbl
    (list (set tbl test))
    (hash-table tbl)))

(defun set= (tbl1 tbl2)
  (iter (for (k _) in-hashtable tbl1)
	(unless (set-member k tbl2) (return-from set= nil)))
  (iter (for (k _) in-hashtable tbl2)
	(unless (set-member k tbl1) (return-from set= nil)))
  t)

(defun union! (tbl1 tbl2 &aux (tbl1 (ensure-set tbl1)))
  (iter-hset (k tbl2) (add! tbl1 k))
  tbl1)

(defun intersection! (tbl1 tbl2 &aux (tbl1 (ensure-set tbl1)))
  (iter-hset (k tbl1)
    (unless (set-member k tbl2)
      (remove! tbl1 k)))
  tbl1)

(defun difference! (tbl1 tbl2 &aux (tbl1 (ensure-set tbl1)))
  (iter-hset (k tbl2)
    (remove! tbl1 k))
  tbl1)

(defun symmetric-difference! (tbl1 tbl2 &aux (tbl1 (ensure-set tbl1)))
  (iter-hset (k tbl2)
    (if (set-member k tbl1)
	(remove! tbl1 k)
	(add! tbl1 k)))
  tbl1)
