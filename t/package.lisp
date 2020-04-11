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

(in-package #:common-lisp-user)

(defpackage "UPAKARANA-TESTS"
  (:use #:common-lisp #:iterate #:alexandria #:fiveam #:upakarana)
  (:export #:test-suite #:run-tests))

;; Create test suite
(5am:def-suite upakarana-tests:test-suite)

(defun upakarana-tests:run-tests ()
  (5am:run! 'upakarana-tests:test-suite))
