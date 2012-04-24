;;; sequence.lisp --- Traversals as sequences.
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


;;; Traversal sequence
;;

(defclass traversal-sequence (standard-object
			      sequence)
  ((traverser :initarg  :traverser
	      :type     t
	      :accessor traverser
	      :documentation
	      ""))
  (:default-initargs
   :traverser (required-argument :traverser))
  (:documentation
   "TODO(): document"))

(defmethod sequence:make-sequence-like ((sequence traversal-sequence)
					(length   integer)
					&rest args &key &allow-other-keys)
  (apply #'sequence:make-sequence-like nil length args))

(defmethod sequence:length ((sequence traversal-sequence))
  (let ((state (traverser sequence)))
    (iter (while (setf state (nth-value 1 (funcall state))))
	  (counting 1))))

(defmethod sequence:elt ((sequence traversal-sequence)
			 (index    integer))
  (let ((state (traverser sequence)))
    (iter (repeat index)
	  (setf state (nth-value 1 (funcall state))))
    (funcall state)))

(defmethod sequence:make-simple-sequence-iterator ((sequence traversal-sequence)
						   &key
						   (start 0)
						   end
						   (from-end nil from-end-supplied?))
  (declare (ignore from-end))
  (when from-end-supplied?
    (error "~S is not supported" :from-end))

  (let ((traverser (traverser sequence)))
    (iter (repeat start)
	  (setf traverser (nth-value 1 (funcall traverser))))
    (multiple-value-call #'vector (funcall traverser) start end)))


;;; Iterator
;;

(defmethod sequence:iterator-element ((sequence traversal-sequence)
				      (iterator simple-array))
  (aref iterator 0))

(defmethod sequence:iterator-step ((sequence traversal-sequence)
				   (iterator simple-array)
				   (from-end t))
  (let+ ((#(nil cont index end) iterator))
    (multiple-value-call #'vector (funcall cont) (1+ index) end)))

(defmethod sequence:iterator-endp ((sequence traversal-sequence)
				   (iterator simple-array)
				   (limit    t)
				   (from-end t))
  (let+ ((#(nil cont index end) iterator))
    (or (null cont) (and end (= index end)))))
