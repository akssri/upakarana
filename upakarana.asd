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

(asdf:defsystem :upakarana
  :licence "GPL"
  :author "See AUTHORS"
  :depends-on (#:iterate #:alexandria)
  :components
  ((:module #:src :components
	    ((:file "package")
	     (:module #:utilities :pathname "" :depends-on ("package") :components
		      ((:file "functions")
		       (:file "macros" :depends-on ("functions"))
		       (:file "set" :depends-on ("macros" "functions"))
		       (:file "vector" :depends-on ("macros" "functions"))
		       (:file "dlist" :depends-on ("macros" "functions"))
		       (:file "union-find" :depends-on ("macros" "functions"))
		       (:file "binary-heap" :depends-on ("macros" "functions"))
		       (:file "fibonacci" :depends-on ("dlist"))))))))
