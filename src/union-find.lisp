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

(in-package #:upakarana-union-find)
;; See https://www.cs.princeton.edu/~rs/AlgsDS07/01UnionFind.pdf

(defclass union-find ()
  ((id :accessor id :initform (make-extensible-vector))
   (size :accessor size :initform (make-extensible-vector))))

(defmethod print-object ((obj union-find) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "size: ~A" (length (id obj)))))

(defun root (i ufd)
  (let ((id (id ufd)))
    (if (= i (length id))
	(prog1 i
	  (vector-push-extend i (id ufd))
	  (vector-push-extend 1 (size ufd)))
	(iter (for ui initially i then (aref id ui))
	      (if (= ui (aref id ui)) (return ui))
	      (setf (aref id ui) (aref id (aref id ui)))))))

(defun unify (i j ufd)
  (let ((id (id ufd)) (size (size ufd))
	(ri (root i ufd)) (rj (root j ufd)))
    (unless (= ri rj)
      (if (< (aref size ri) (aref size rj))
	  (rotatef ri rj))
      (setf (aref id rj) ri
	    (aref size ri) (+ (aref size ri) (aref size rj))
	    (aref size rj) -1)))
  ufd)
