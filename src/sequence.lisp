;;;; sequence.lisp --- Traversals as sequences.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:traverse)

;;; Traversal sequence

(defclass traversal-sequence (standard-object
                              sequence)
  ((traverser :initarg  :traverser
              :type     t
              :accessor traverser
              :documentation
              "Stores a pure `traverser' which is positioned at the
               beginning of a traversal."))
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

(defmethod length> ((query    integer)
                    (sequence traversal-sequence))
  "TODO(): document"
  (let ((state (traverser sequence)))
    (iter (while (setf state (nth-value 1 (funcall state))))
          (for i :from 0)
          (when (= i query)
            (return t)))))

(defmethod sequence:elt ((sequence traversal-sequence)
                         (index    integer))
  (let ((state (traverser sequence)))
    (iter (repeat index)
          (setf state (nth-value 1 (funcall state))))
    (funcall state)))

(defmethod (setf sequence:elt) ((new-value t)
                                (sequence  traversal-sequence)
                                (index     integer))
  (error 'simple-program-error
         "~@<Sequence ~A is immutable.~@:>"
         sequence))

(defmethod sequence:make-simple-sequence-iterator ((sequence traversal-sequence)
                                                   &key
                                                   (start 0)
                                                   end
                                                   from-end)
  (when from-end
    (error "~S is not supported" :from-end))

  (let ((traverser (traverser sequence)))
    (iter (repeat start)
          (setf traverser (nth-value 1 (funcall traverser))))
    (multiple-value-call #'vector (funcall traverser) start end)))

(defmethod print-object ((object traversal-sequence) stream)
  "TODO(): document"
  (print-unreadable-object (object stream :type t :identity t)
    (let ((length (apply #'min (or *print-length* 5)
                         (unless (length> 5 object)
                           (list (length object))))))
      (format stream "~:[<empty>~;~:*~{~A~^ ~}~]~:[~; ...~]"
              (when (plusp length)
                (coerce (subseq object 0 length) 'list))
              (length> length object)))))

;;; Sequence functions

(defmethod #|sequence:|# map1 ((result-type (eql 'traversal-sequence))
                               (func        t)
                               (sequence    traversal-sequence)
                               &rest
                               sequences)
  "TODO(): document"
  (generating
   (iter (for element each sequence)
         (yield (funcall func element)))))

(defmethod sequence:remove-if ((pred t) (sequence traversal-sequence)
                               &key
                               (start 0)
                               end
                               from-end
                               key
                               count)
  "TODO(): document"
  (let ((filter (if key (compose pred key) (coerce pred 'function))))
    (declare (type function filter))
    (generating
      (if count
          (iter (for      element each sequence :from start) ; TODO end
                (generate i :from 1)
                (while (or (not count) (< i count)))
                (unless (funcall filter element)
                  (next i)
                  (yield element)))

          (iter (for element each sequence :from start)
                (unless (funcall filter element)
                  (yield element)))))))

#+much-faster
(let ((c (traverser sequence)))
  (declare (type function c))
  (loop
     (multiple-value-bind (v cont) (funcall c)
       (setf c cont)
       (yield v))))

;;; Iterator

(defmethod sequence:iterator-element ((sequence traversal-sequence)
                                      (iterator simple-array))
  (declare #+later (optimize (debug 0) (safety 0) (speed 3))
           (type (simple-array t (4)) iterator))
  (aref iterator 0))

(defmethod sequence:iterator-step ((sequence traversal-sequence)
                                   (iterator simple-array)
                                   (from-end t))
  (declare #+later (optimize (debug 0) (safety 0) (speed 3))
           (type (simple-array t (4)) iterator)) ; TODO type
  (let+ ((#(&ign cont index end) iterator))
    (declare (type function cont)
             (type non-negative-integer index))
    (multiple-value-call #'vector (funcall cont) (1+ index) end)))

(defmethod sequence:iterator-endp ((sequence traversal-sequence)
                                   (iterator simple-array)
                                   (limit    t)
                                   (from-end t))
  (declare #+later (optimize (debug 0) (safety 0) (speed 3))
           (type (simple-array t (4)) iterator)) ; TODO type
  (let+ ((#(&ign cont index end) iterator))
    (declare (type (or null function) cont)
             (type non-negative-integer index)
             (type (or null non-negative-integer) end))
    (or (null cont) (and end (= index end)))))
