;;;; operations.lisp --- Sequence operations.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:utilities.sequence)

;;; Sequence functions

(defmethod sequence:subseq ((sequence sequence-proxy-mixin)
                            start &optional end)
  (let ((iterator
          (multiple-value-call #'vector
            sequence
            (multiple-value-bind (iterator limit from-end step endp elt setelt index copy) ;TODO
                (sequence:make-sequence-iterator
                 sequence :start start :end end)
              (declare (ignore from-end))
              (values iterator limit step endp elt setelt index copy)))))
    (make-instance (class-of sequence) :iterator iterator))) ; TODO proper cloning

(defmethod sequence:map ((result-prototype sequence-proxy-mixin)
                         (function         t)
                         (sequence         t)
                         &rest
                         sequences)
  (cond
    ((null sequences)
     (generating-sequence
       (iter (for element each sequence)
             (yield (funcall function element)))))
    (t
     (let+ ((all (list* sequence sequences))
            ((&flet some-endp (iterators)
               (some (lambda (iterator)
                       (let ((sequence (aref iterator 0))
                             (iterator (aref iterator 1))
                             (limit    (aref iterator 2))
                             (endp     (aref iterator 4)))
                         (funcall endp sequence iterator limit nil)))
                     iterators)))
            ((&flet all-step (iterators)
               (mapc (lambda (iterator*)
                       (symbol-macrolet ((sequence (aref iterator* 0))
                                         (iterator (aref iterator* 1))
                                         (limit    (aref iterator* 2))
                                         (step     (aref iterator* 3)))
                         (setf iterator (funcall step sequence iterator limit))))
                     iterators)))
            ((&flet all-elt (iterators args)
               (map-into args (lambda (iterator)
                                (let ((sequence (aref iterator 0))
                                      (iterator (aref iterator 1))
                                      (elt      (aref iterator 5)))
                                  (funcall elt sequence iterator)))
                         iterators))))
       ;; TODO (declare (dynamic-extent all))

       ;; Having this second let inside `generating-sequence' is
       ;; necessary for the resulting iterators to be copyable.
       (generating-sequence
         (let ((iterators (progn #+TODO cl-cont:without-call/cc
                                 (mapcar (lambda (sequence)
                                           (multiple-value-call #'vector
                                             sequence
                                             (multiple-value-bind (iterator limit from-end step endp elt setelt index copy) ;TODO
                                                 (sequence:make-sequence-iterator sequence)
                                               (declare (ignore from-end))
                                               (values iterator limit step endp elt setelt index copy))))
                                         all)))
               (args       (make-list (length all))))
           ;; TODO (declare (dynamic-extent all args))
           (iter (until (some-endp iterators))
                 (all-elt iterators args)
                 (yield (apply function args))
                 (all-step iterators))))))))

(defmethod sequence:remove-if ((pred t) (sequence sequence-proxy-mixin)
                               &key
                               (start 0)
                               end
                               from-end
                               key
                               count)
  "TODO(): document"
  (when from-end
    (error "~S is not supported" :from-end))

  (let ((filter (if key (compose pred key) (ensure-function pred))))
    (declare (type function filter))
    (if count
        (generating-sequence
          (iter (for      element each sequence :from start :to end) ; TODO end
                (generate i :from 1)
                (if (and (< i count) (funcall filter element))
                    (next i)
                    (yield element))))
        (generating-sequence
          (iter (for element each sequence :from start :to end)
                (unless (funcall filter element)
                  (yield element)))))))

#+much-faster
(let ((c (traverser sequence)))
  (declare (type function c))
  (loop
    (multiple-value-bind (v cont) (funcall c)
      (setf c cont)
      (yield v))))

#+later (defmethod sequence:substitute-if
            ((new t) (predicate t) (sequence sequence-proxy-mixin)
             &key (start 0) end from-end count key)
          (when from-end
            (error "~S is not supported" :from-end)) ; TODO macro

          (with-traverser-at ((state next :start start) sequence) ; TODO let+
            (let+ ((key (sequence:canonize-key key))
                   ((&flet+ next/count ((state . remaining))
                      (let+ (((&values value new-state) (funcall next state))
                             (new-value
                              (if (and (plusp remaining)
                                       (funcall predicate (funcall key value)))
                                  (progn
                                    (decf remaining)
                                    new)
                                  value)))
                        (values new-value (cons new-state remaining)))))
                   ((&flet next/no-count (state)
                      (let+ (((&values value new-state) (funcall next state))
                             (new-value
                              (if (funcall predicate (funcall key value))
                                  new
                                  value)))
                        (values new-value new-state)))))
      ; TODO end
      (make-instance 'traversal-sequence
                     :state (if count (cons state count) state)
                     :next  (if count #'next/count #'next/no-count)))))

(defmethod sequence:replace ((sequence1 sequence-proxy-mixin) (sequence2 sequence-proxy-mixin)
                             &key start1 end1 start2 end2)
  )

(defmethod sequence:concatenate ((result-prototype sequence-proxy-mixin)
                                 &rest sequences)
  (case (length sequences)
    (0 +the-empty-sequence-proxy-mixin-sequence+)
    (1 (first sequences))
    (t
     (generating-sequence
       (iter (for sequence in sequences)
             (iter (for element each sequence)
                   (yield element)))))))

(defmethod concatenate/2 ((result-prototype sequence-proxy-mixin)
                          (sequence1        sequence-proxy-mixin)
                          (sequence2        sequence-proxy-mixin))
  )

#+no (locally
    (declare (optimize (sb-c::store-xref-data 0)))
  (defmethod sequence:merge ((result-prototype sequence-proxy-mixin)
                             (sequence1        sequence)
                             (sequence2        sequence)
                             (predicate        function)
                             &key key)  ; TODO fdefinition
    (generating-sequence
      (iter (generate element1 each sequence1)
            (generate key1 next (if key
                                    (funcall key (next element1))
                                    (next element1)))
            (generate element2 each sequence2)
            (generate key2 next (if key
                                    (funcall key (next element2))
                                    (next element2)))
            (when (first-iteration-p)
              (next key1)
              (next key2))
            (if (funcall predicate key1 key2)
                (progn
                  (yield element1)
                  (next key1))
                (progn
                  (yield element2)
                  (next key2)))))))
