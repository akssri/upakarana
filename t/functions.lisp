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

(5am:def-suite :functions :in test-suite)
(5am:in-suite :functions)

(5am:test binary-search-test
  ;; test invariant
  (letv* ((n 1000) (order #'<)
	  (a (sort (iter (repeat n) (collect (random 1d0) result-type vector)) order)))
    (is (progn
	  (iter (for ii below n)
	    (let* ((x (if (= 0 (random 2)) (aref a ii) (random 1d0)))
		   (idx (binary-search a x :order order)))
	      (always (or (= idx n) (not (funcall order (aref a idx) x)))))))))
  ;; side: left
  (is (= 0 (binary-search #(1 1 1) 1 :order #'<)))
  (is (= 1 (binary-search #(0 1 1 1) 1 :order #'<)))
  ;; side: right
  (is (= 4 (binary-search #(0 1 1 1) 1 :order #'<=)))
  (is (= 4 (binary-search #(0 1 1 1 2) 1 :order #'<=)))
  ;; bounds
  (is (= 4 (binary-search #(0 1 1 1) 1.5 :order #'<)))
  (is (= 4 (binary-search #(0 1 1 1 2) 1.5 :order #'<))))
