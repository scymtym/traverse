;;; examples.lisp ---
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

(defmethod successors ((node package))
  (let (result)
    (do-external-symbols (symbol node)
      (push symbol result))
    result))

(defmethod successors ((node symbol))
  (unless (member node '(nil t *package*))
    (append (when (and (boundp node)
		       (not (eq (symbol-value node) node)))
	      (list (symbol-value node)))
	    (when (fboundp node)
	      (list (fdefinition node))))))

(defmethod successors ((node number))
  (list (1- node) (1+ node)))

(subseq
 (make-instance 'traversal-sequence
		:traverser (make-traverser/pure 0 :bfs))
 0 100)
