;;; traverse.asd --- System definition for traverse system.
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:defpackage :traverse-system
  (:use
   :cl
   :asdf))


(cl:in-package :traverse-system)

(defsystem :traverse
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "LLGPLv3; see COPYING file for details."
  :description "Interfaces for traversing arbitrary structures."
  :depends-on  (:alexandria
		:let-plus
		:iterate
		:cl-cont)
  :components  ((:module     "src"
		 :components ((:file       "package")

			      (:file       "types"
			       :depends-on ("package"))
			      (:file       "protocol"
			       :depends-on ("package"))

			      (:file       "builtins"
			       :depends-on ("package" "protocol"))))))
