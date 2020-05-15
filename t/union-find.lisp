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

(5am:def-suite :union-find :in test-suite)
(5am:in-suite :union-find)

(5am:test union-find-test
  (let* ((N 1000) (M N)
	 (ufd (make-instance 'u.ufd:union-find))
	 (tbl (make-hash-table)))
    (iter (for ii below N)
	  (setf (gethash ii tbl) (list ii))
	  (u.ufd:root ii ufd))
    (iter (repeat M)
	  (let* ((ii (random N)) (jj (random N))
		 (newset (append (gethash ii tbl) (gethash jj tbl))))
	    (u.ufd:unify ii jj ufd)
	    (setf (gethash ii tbl) newset
		  (gethash jj tbl) newset)))
    (is (progn
	  (iter (for (k set-k) in-hashtable tbl)
	    (always (and
		     (= 1 (length (remove-duplicates (mapcar #'(lambda (x) (u.ufd:root x ufd)) set-k))))
		     (null (set-difference set-k (u.ufd:subset k ufd))))))))))
