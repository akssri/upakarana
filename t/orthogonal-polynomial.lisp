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

(5am:def-suite :orthogonal-polynomial :in test-suite)
(5am:in-suite :orthogonal-polynomial)

(5am:test imtqlx
  (letv* ((dn (make-array 8 :element-type 'double-float))
	  (en (make-array 8 :element-type 'double-float :initial-contents
			  '(0.5773502691896257d0 0.5163977794943222d0 0.50709255283711d0 0.5039526306789697d0 0.502518907629606d0 0.5017452060042545d0 0.501280411827603d0 0.0d0)))
	  (zn (make-array 8 :element-type 'double-float)))
    (setf (aref zn 0) 1d0)
    (u.opoly:imtqlx dn en zn)
    (is
     (equalp (list dn zn)
	     (list
	      #(-0.9602898564975365d0 -0.796666477413627d0 -0.525532409916329d0
		-0.18343464249564978d0 0.1834346424956496d0 0.5255324099163292d0
		0.7966664774136268d0 0.9602898564975364d0)
	      #(0.22497615016971922d0 -0.3334524212338056d0 0.39604712211925436d0
		-0.42584256678869153d0 0.4258425667886916d0 -0.3960471221192542d0
		0.33345242123380525d0 0.2249761501697189d0))))))

(5am:test legendre-quadrature
  (letv* ((p (u.opoly:orthogonal-polynomial :legendre))
	  (n 16) (eps 1d-14)
	  (x w (u.opoly:gauss-quadrature n p)))
    ;; root
    (is (progn
	  (iter (for xi in-vector x with-index i)
	    (always (< (abs (u.opoly:evaluate xi n p)) eps)))))
    ;; weight
    (is (progn
	  (iter (for xi in-vector x with-index i)
	    (let ((wi-analytical (/ 2 (- 1 (* xi xi)) (expt (nth-value 1 (u.opoly:evaluate xi n p 1)) 2))))
	      (always (< (abs (- wi-analytical (aref w i))) eps))))))))

(5am:test chebyshev-t-quadrature
  (letv* ((p (u.opoly:orthogonal-polynomial :chebyshev-t))
	  (n 16) (eps 1d-13)
	  (x w (u.opoly:gauss-quadrature n p)))
    ;; root
    (is (progn
	  (iter (for xi in-vector x with-index i)
	    (let ((xi-analytical (- (cos (* pi (- (* 2 (1+ i)) 1) (/ (* 2 n)))))))
	      (always (and (< (abs (u.opoly:evaluate xi n p)) eps)
			   (< (abs (- xi-analytical xi)) eps)))))))
    ;; weight
    (is (progn
	  (iter (for xi in-vector x with-index i)
	    (let ((wi-analytical (/ pi n)))
	      (always (< (abs (- wi-analytical (aref w i))) eps))))))))

(5am:test chebyshev-u-quadrature
  (letv* ((p (u.opoly:orthogonal-polynomial :chebyshev-u))
	  (n 16) (eps 1d-12)
	  (x w (u.opoly:gauss-quadrature n p)))
    ;; root
    (is (progn
	  (iter (for xi in-vector x with-index i)
	    (let ((xi-analytical (cos (* pi (/ (- n i) (1+ n))))))
	      (always (and (< (abs (u.opoly:evaluate xi n p)) eps)
			   (< (abs (- xi-analytical xi)) eps)))))))
    ;; weight
    (is (progn
	  (iter (for xi in-vector x with-index i)
	    (let ((wi-analytical (* (/ pi (1+ n)) (expt (sin (* pi (/ (- n i) (1+ n)))) 2))))
	      (always (< (abs (- wi-analytical (aref w i))) eps))))))))
