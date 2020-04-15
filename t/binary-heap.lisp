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

(in-package #:upakarana-tests)

(5am:def-suite :binary-heap :in test-suite)
(5am:in-suite :binary-heap)

(5am:test binary-heap-test
  (let* ((N 1000)
	 (heap (make-extensible-vector))
	 (order #'<))
    ;; insert
    (iter (repeat N) (u.bheap:push (random 1d0) heap))
    ;; test
    (is
     (iter (repeat N)
	   (with prev-min = nil)
	   (let ((cur-min (u.bheap:pop heap)))
	     ;; check ordering
	     (if (and prev-min (not (funcall order prev-min cur-min)))
		 (return nil))
	     (setf prev-min cur-min))
	   (finally (return t))))))
