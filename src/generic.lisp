;;;; generic.lisp --- Traversal via successors protocol.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:traverse)

(defmethod make-traverser/pure ((structure t)
                                (traversal (eql :dfs)))
  (lambda ()
    (with-yield
      (labels ((next* (node)
                 (yield node)
                 (progn #+maybe cont:without-call/cc
                        (iter (for successor each (successors node))
                              (next* successor)))))
        #+old (lambda () (next* structure))
        (next* structure)))))

(defmethod make-traverser/pure ((structure t)
                                (traversal (eql :bfs)))
  (lambda ()
    (with-yield
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
                       (yield node)
                       (next (rest head) last))
                     (values nil nil))))
        (let ((queue (list structure)))
          #+old (curry #'next queue queue)
          (next queue queue))))))
