;;;; builtins.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:traverse)

;;; Cons

(defmethod make-traverser/pure ((structure cons)
                                (traversal (eql :dfs)))
  (with-yield
    (labels ((next (cell)
               (yield cell)
               (if (atom cell)
                   (values nil nil)
                   (progn
                     (next (car cell))
                     (next (cdr cell))))))
      (curry #'next structure))))

(defmethod make-traverser/pure ((structure cons)
                                (traversal (eql :bfs)))
  (with-yield
    (labels ((next (head last)
               (if head
                   (progn
                     (when (consp (car head))
                       (let+ (((car . cdr) (car head)))
                         (unless (eq (cadr last) car)
                           (setf (cdr last) (cons car (cons cdr nil))))
                         (setf last (cddr last))))
                     (yield (car head))
                     (next (rest head) last))
                   (values nil nil))))
      (let ((queue (list structure)))
        (curry #'next queue queue)))))

;;; Vector

(defmethod make-traverser/pure ((structure vector)
                                (traversal (eql :dfs)))
  (lambda ()
    (with-yield
      (iter (for element in-vector structure)
            (yield element)))))

(sb-sprof:with-profiling ()
  (let ((s (make-array 10000000)))
    (time
     (let ((tr (make-traverser/pure s :dfs)))
       (count 0 (make-instance 'traversal-sequence :traverser tr))))))
