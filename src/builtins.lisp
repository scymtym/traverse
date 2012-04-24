;;; builtins.lisp ---
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


;;; Cons
;;

(defmethod make-traverser/pure ((structure cons)
				(traversal (eql :dfs)))
  (cont:with-call/cc
    (labels ((next (cell)
	       (cont:let/cc k
		 (values cell k))
	       (if (atom cell)
		   (values nil nil)
		   (progn
		     (next (car cell))
		     (next (cdr cell))))))
      (curry #'next structure))))

(defmethod make-traverser/pure ((structure cons)
				(traversal (eql :bfs)))
  (cont:with-call/cc
    (labels ((next (head last)
	       (if head
		   (progn
		     (when (consp (car head))
		       (let+ (((car . cdr) (car head)))
			 (unless (eq (cadr last) car)
			   (setf (cdr last) (cons car (cons cdr nil))))
			 (setf last (cddr last))))
		     (cont:let/cc k
		       (values (car head) k))
		     (next (rest head) last))
		   (values nil nil))))
      (let ((queue (list structure)))
	(curry #'next queue queue)))))
