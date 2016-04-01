(cl:in-package #:utilities.sequence)

;;; Tests

(defun foo (x)
  x)

(let+ (((&values state next) (with-yield
                               (iter (for i :from 0) (yield i))))
       (s (make-generator-sequence state next)))
  (length> 5 s)
  (sb-sequence:with-sequence-iterator-functions (step1 endp1 elt1 setelt index copy) (s :end 10)
    (print (index))
    (print (elt1))
    (step1)
    (print (index))
    (print (elt1))
    (step1)
    (print (index))
    (print (elt1))
    (step1)
    (print (index))
    (print (elt1)))
  (print (map 'generator-sequence (lambda (x) (+ x 5)) (remove-if #'oddp s))))

(defun g ()
  ;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((s  (generating-sequence (iter (for i :from 0) (yield i))))
         (s2 (remove-if #'oddp s))
         (s3 (remove-if #'evenp s)))
    (print (map 'generator-sequence (lambda (x y z) (+ x y z 5)) s2 s3 (make-list 10 :initial-element 1)))
    (print (map 'generator-sequence #'list s2 s3 (make-list 10 :initial-element 1)))
    (print (map 'generator-sequence #'list (subseq s2 10000) (subseq s3 10000)))))

#+no (time
      )

(defun f ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((state (with-yield
                 (iter (for (the fixnum i) :from 0 :to 10000000) (yield i)))))
    (declare (type function state))
    (loop :repeat 10000000 :do (setf state (the function (nth-value 1 (funcall state)))))))

(time
 (sb-sprof:with-profiling ()
   (g)))

(let* ((s  (generating-sequence (iter (for i :from 0) (yield i))))
       (s2 (remove-if #'oddp s))
       (s3 (remove-if #'evenp s)))
  (merge (type-of s2) s2 s3 #'<))
