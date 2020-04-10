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

(defpackage "UPAKARANA"
  (:use #:common-lisp #:iterate #:alexandria)
  (:export #:make-extensible-vector #:maptree-if #:maptree #:cartesian-product #:mapcart
	   #:pair #:zip #:unzip #:ziptree #:recursive-append #:take-while
	   #:binary-search #:sort-index #:topological-sort
	   ;; macros
	   #:gethash! #:assoc! #:getf! #:accum! #:values-n #:letv* #:cart-case #:cart-ecase
	   #:cart-typecase #:cart-etypecase #:with-memoization #:.>))

(defpackage "UPAKARANA-UNION-FIND"
  (:nicknames :u.ufd)
  (:use #:common-lisp #:upakarana #:iterate)
  (:export #:union-find #:id #:size #:root #:unify))

(defpackage "UPAKARANA-HASH-SET"
  (:nicknames :u.set)
  (:use #:common-lisp #:upakarana #:iterate)
  (:shadow #:set)
  (:export #:set #:ensure-set #:set->list #:set-member #:set= #:add! #:union! #:intersection! #:difference! #:symmetric-difference!))

(defpackage "UPAKARANA-DOUBLY-LINKED-LIST"
  (:nicknames #:u.dlist)
  (:use #:common-lisp #:upakarana #:iterate #:alexandria)
  (:export #:dcons #:dcar #:dcdr #:drdc #:dcons! #:dring #:dlist #:dlast #:dpush #:dpop))

(defpackage "UPAKARANA-FIBONACCI-HEAP"
  (:nicknames :u.fheap)
  (:use #:common-lisp #:upakarana #:upakarana-doubly-linked-list #:iterate #:alexandria)
  (:export #:fibonacci-heap #:min-value #:node-value #:node-existsp #:insert #:extract-min #:decrease-key #:delete-key))

(defpackage "UPAKARANA-BINARY-HEAP"
  (:nicknames :u.bheap)
  (:use #:common-lisp #:upakarana #:iterate #:alexandria)
  (:export #:make-heap #:heappush #:heappop #:heapify-up #:heapify-down #:parent-index #:lchild-index))

(defpackage "UPAKARANA-VECTOR"
  (:nicknames :u.vec)
  (:use #:common-lisp #:upakarana)
  (:export #:foldr #:foldl #:extreme #:copy! #:map!))
