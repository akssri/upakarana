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

(in-package #:upakarana-binary-heap)

(declaim (inline parent-index))
(defun parent-index (idx)
  (floor (1- idx) 2))

(declaim (inline lchild-index))
(defun lchild-index (idx)
  (+ 1 (* 2 idx)))

(defun heapify-up (idx vec &optional (order #'<))
  (when (< 0 idx)
    (let ((p-idx (parent-index idx)))
      (unless (funcall order (aref vec p-idx) (aref vec idx))
	(rotatef (aref vec p-idx) (aref vec idx))
	(heapify-up p-idx vec order)))))

(defun heapify-down (idx vec &optional (order #'<))
  (let* ((l-idx (lchild-index idx))
	 (r-idx (1+ l-idx))
	 (swap-idx idx))
    (when (and (< l-idx (length vec)) (not (funcall order (aref vec swap-idx) (aref vec l-idx))))
      (setf swap-idx l-idx))
    (when (and (< r-idx (length vec)) (not (funcall order (aref vec swap-idx) (aref vec r-idx))))
      (setf swap-idx r-idx))
    (when (/= swap-idx idx)
      (rotatef (aref vec idx) (aref vec swap-idx))
      (heapify-down swap-idx vec order))))

(defun heappush (item vec &optional (order #'<))
  (heapify-up (vector-push-extend item vec) vec order))

(defun heappop (vec &optional (order #'<))
  (when (< 0 (length vec))
    (rotatef (aref vec 0) (aref vec (1- (length vec))))
    (prog1 (vector-pop vec)
      (heapify-down 0 vec order))))

(defun make-heap (seq &optional (order #'<))
  (let ((vec (make-extensible-vector)))
    (map nil #'(lambda (x) (vector-push-extend x vec)) seq)
    (loop :for ii :from (parent-index (1- (length seq))) :downto 0
       :do (heapify-down ii vec order))
    vec))
