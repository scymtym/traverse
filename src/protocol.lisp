;;; protocol.lisp ---
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

(cl:in-package :traverse)


;;; Client protocol
;;

(defgeneric map-structure (result-type function structure)
  (:documentation
   "Apply FUNCTION to all traversed elements in STRUCTURE collecting
   and returning results as a sequence of type RESULT-TYPE."))

(defmethod map-structure ((result-type symbol)
			  (function    function)
			  (structure   t))

  (let ((traverser (make-traverser structure :dfs))
	(visitor   (make-visitor   function  t)))
    (iter (for (event . state) next (funcall traverser))
	  (until (eq event :end))
	  (funcall visitor event))))


;;; Traversal protocol
;;

(defgeneric make-traverser/pure (structure traversal)
  (:documentation
   "Return a pure traverser for STRUCTURE which performs a traversal
    according to TRAVERSAL.

    Since the returned traverser is pure, it can be stored and called
    multiple times."))

(defgeneric make-traverser/stateful (structure traversal)
  (:documentation
   "Return a stateful traverser for STRUCTURE which performs a
   traversal according to TRAVERSAL.

   Since the returned traverser is stateful, it can only be called
   once."))

(defmethod make-traverser/stateful ((structure t)
				    (traversal t))
  ;; If a pure traverser can be constructed for STRUCTURE, do that and
  ;; turn the result into a stateful traverser.
  (let ((state (make-traverser/pure structure traversal)))
    (declare (type traverser state))
    #'(lambda ()
	(let+ (((&values value new-state) (funcall state)))
	  (setf state new-state)
	  value))))


;;; Simple traversal protocol
;;

(defgeneric successors (node)
  (:documentation
   "Return the successors of NODE."))

(defmethod successors ((node t))
  nil)

(defgeneric make-visitor (function events)
  (:documentation
   "TODO"))
