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

(5am:def-suite :fourier-motzkin :in test-suite)
(5am:in-suite :fourier-motzkin)

(5am:test fourier-motzkin-test
  (let ((A (map 'vector #'(lambda (x)
			    (letv* (((coeff op) x))
			      (u.fm:make-inequation :row (coerce coeff '(simple-array u.fm:inequation-dtype (*))) :op op)))
		'(((1 0 10) :<=)       ;; i <= 10
		  ((1 0 0) :>=)        ;; i >= 0
		  ((0 1 5) :<=)        ;; j <= 5
		  ((0 1 0) :>=)        ;; j >= 0
		  ((1 -1 0) :<=)))))   ;; i - j <= 0
    (letv* ((solution A-next (u.fm:fourier-motzkin 1 A))) ;; eliminate j
      (flet ((inequation (x) (list (coerce (u.fm:inequation-row x) 'list) (u.fm:inequation-op x))))
	(is
	 (and (equal
	       (map 'list #'inequation solution)
	       '(((0 1 5) :<=)         ;; j <= 5
		 ((0 1 0) :>=)         ;; j >= 0
		 ((1 -1 0) :<=)))      ;; j >= i (-> j >= max(0, i))
	      (equal
	       (map 'list #'inequation A-next)
	       '(((-1 0 0) :<=)        ;; i >= 0
		 ((1 0 5) :<=))))))))) ;; i <= 5
