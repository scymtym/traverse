;;; generic.lisp --- Traversal via successors protocol.
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

(defmethod make-traverser/pure ((structure t)
				(traversal (eql :dfs)))
  (cont:with-call/cc
    (labels ((next* (node)
	       (cont:let/cc k
		 (values node k))
	       (iter (for successor in (successors node))
		     (next* successor))
	       (values nil nil)))
      #'(lambda () (next* structure)))))

(defmethod make-traverser/pure ((structure t)
				(traversal (eql :bfs)))
  (cont:with-call/cc
    (labels ((next (head last)
	       (if head
		   (let* ((node       (car head))
			  (successors (successors node)))
		     (cond
		       ((null successors))
		       ((eq (cadr last) (first successors))
			(setf last (nthcdr (length successors) last)))
		       (t
			(iter (for child in successors)
			      (setf (cdr last) (cons child nil)
				    last       (cdr last)))))
		     (cont:let/cc k
		       (values node k))
		     (next (rest head) last))
		   (values nil nil))))
      (let ((queue (list structure)))
	(curry #'next queue queue)))))
