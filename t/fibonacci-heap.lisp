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

(5am:def-suite :fibonacci-heap :in test-suite)
(5am:in-suite :fibonacci-heap)

(5am:test fibonacci-heap-test
  (let ((N 1000)
	(fib (make-instance 'u.fheap:fibonacci-heap)))
    ;; insert
    (iter (repeat N) (u.fheap:push (random 1d0) fib))
    ;; random decrement
    (iter (for ii below N)
	  (handler-case (u.fheap:decrement (random 1d0) ii fib)
	    (u.fheap:fibonacci-heap-decrement-infeasible () nil)))
    ;; test
    (is
     (iter (repeat N)
	   (with prev-min = nil)
	   (let ((cur-min (u.fheap:pop fib)))
	     ;; check ordering
	     (if (and prev-min (not (funcall (u.fheap:order fib) prev-min cur-min)))
		 (return nil))
	     (setf cur-min prev-min))
	   (finally (return t))))))
