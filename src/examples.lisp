;;;; examples.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:traverse)

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
