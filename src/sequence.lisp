;;;; sequence.lisp --- Traversals as sequences.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:traverse)

;;; Traversal sequence

(defclass traversal-sequence (standard-object
                              sequence)
  ((state :initarg  :state
          :type     t
          :accessor sequence-state
          :documentation
          "TODO")
   (next  :initarg  :next
          :type     function
          :accessor sequence-next
          :documentation
          "TODO"))
  (:default-initargs
   :state (required-argument :state)
   :next  (required-argument :next))
  (:documentation
   "TODO(): document"))

(defmacro with-traverser (((state-var next-var) sequence) &body body)
  "TODO(jmoringe): document"
  (check-type state-var symbol)
  (check-type next-var symbol)

  `(let+ (((&accessors (,state-var sequence-state)
                       (,next-var sequence-next)) sequence))
     ,@body))

(defmacro with-traverser-r/o (((state-var next-var) sequence) &body body)
  "TODO(jmoringe): document"
  (check-type state-var symbol)
  (check-type next-var symbol)

  `(let ((,state-var (sequence-state sequence))
         (,next-var  (sequence-next sequence)))
     (declare (type function ,next-var))
     ,@body))

(defmacro with-traverser-at (((state-var next-var &key start) sequence) &body body)
  "TODO(jmoringe): document"
  (check-type state-var symbol)
  (check-type next-var symbol)

  `(let ((,state-var (sequence-state sequence))
         (,next-var  (sequence-next sequence)))
     (declare (type function ,next-var))
     (iter (repeat ,start)
           (setf ,state-var
                 (nth-value 1 (funcall ,next-var ,state-var))))
     ,@body))

(defmethod sequence:make-sequence-like
    ((sequence traversal-sequence)
     (length   t)
     &key
     (initial-element nil initial-element-supplied?)
     (initial-contents nil initial-contents-supplied?)
     &allow-other-keys)
  (let+ (((&values state next)
          (cond
            ((and (not initial-element-supplied?)
                  (not initial-contents-supplied?))
             (traverser sequence))
            (initial-element-supplied?
             (values
              nil
              (labels ((constantly2 (state)
                         (values initial-element state)))
                #'constantly2)))
            (initial-contents-supplied?
             (with-yield ;; TODO make a function
               (iter (for element each initial-contents)
                     (yield element))))
            (t
             (error "~S and ~S are mutually exclusive"
                    :initial-element :initial-contents)))))
    (if length
        (make-instance 'traversal-sequence-with-limit
                       :state state
                       :next  next
                       :limit length)
        (make-instance 'traversal-sequence
                       :state state
                       :next  next))))

#+no (defmethod #+not-yet sequence: emptyp ((sequence traversal-sequence))

  )

(defmethod sequence:length ((sequence traversal-sequence))
  (with-traverser-r/o ((state next) sequence)
    (iter (while (setf state (nth-value 1 (funcall next state))))
          (counting 1))))

(defmethod #+not-yet sequence: length> ((query    integer)
                    (sequence traversal-sequence))
  (with-traverser-r/o ((state next) sequence)
    (iter (while (setf state (nth-value 1 (funcall next state))))
          (for i :from 0)
          (when (= i query)
            (return t)))))

(defmethod sequence:elt ((sequence traversal-sequence)
                         (index    integer))
  (with-traverser-r/o ((state next) sequence)
    (iter (repeat index)
          (setf state (nth-value 1 (funcall next state))))
    (values (funcall next state))))

(defmethod (setf sequence:elt) ((new-value t)
                                (sequence  traversal-sequence)
                                (index     integer))
  (error 'simple-program-error
         "~@<Sequence ~A is immutable.~@:>"
         sequence))

(defmethod sequence:make-sequence-iterator ((sequence traversal-sequence)
                                            &key
                                            (start 0)
                                            end
                                            from-end)
  (when from-end
    (error "~S is not supported" :from-end))

  (with-traverser-r/o ((state next) sequence)
    (let ((index start)
          (value))
      ;; Advance to desired start position.
      (iter (repeat (1+ start))
            (multiple-value-setq (value state) (funcall next state))
            (unless state
              (error "Start index ~D is not within bounds of ~A"
                     start sequence)))

      ;; Return iterator state with reader and mutator closures.
      (values state end from-end
              (named-lambda iterator-next (sequence state limit)
                (declare (ignore sequence limit))
                (incf index)
                (multiple-value-setq (value state) (funcall next state))
                state)
              (if end
                  (named-lambda iterator-endp/end (sequence state limit from-end)
                    (declare (ignore sequence limit from-end))
                    (or (= index end) (null state)))
                  (named-lambda iterator-endp/no-end (sequence state limit from-end)
                    (declare (ignore sequence limit from-end))
                    (null state)))
              (named-lambda iterator-element (sequence state)
                (declare (ignore sequence state))
                value)
              (named-lambda (setf iterator-element) (new-value sequence state)
                (declare (ignore new-value state))
                (error "Sequence ~A is immutable."
                       sequence))
              (named-lambda iterator-index (sequence state)
                (declare (ignore sequence state))
                index)
              (named-lambda iterator-copy (sequence state)
                (declare (ignore sequence))
                state)))))

#+old (defmethod sequence:make-simple-sequence-iterator ((sequence traversal-sequence)
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
  (print-unreadable-object (object stream :type t :identity t)
    (let ((length (apply #'min (or *print-length* 5)
                         (unless (length> 5 object)
                           (list (length object))))))
      (format stream "~:[<empty>~;~:*~{~A~^ ~}~]~:[~; ...~]"
              (when (plusp length)
                (coerce (subseq object 0 length) 'list))
              (length> length object)))))

;;; Sequence functions

;; TODO should we have another method for the case SEQUENCE is-a `traversal-sequence'?

(defmethod sequence:map ((result-prototype traversal-sequence)
                         (function         t)
                         (sequence         sequence)
                         &rest
                           sequences)
  (assert (null sequences)) ; TODO for now
  (let ((sequences (list* sequence sequences)))
    (declare (dynamic-extent sequences))
    (generating
      (iter (for element each sequence)
            (yield (funcall func element))))))

(defmethod sequence:subseq ((sequence traversal-sequence) start &optional end)
  (with-traverser-at ((state next :start start) sequence)
    (if end
        (make-instance 'traversal-sequence-with-limit
                       :state state
                       :next  next
                       :limit (- end start))
        (make-instance 'traversal-sequence
                       :state state
                       :next  next))))

(defmethod sequence:remove-if ((pred t) (sequence traversal-sequence)
                               &key
                               (start 0)
                               end
                               from-end
                               key
                               count)
  "TODO(): document"
  (when from-end
    (error "~S is not supported" :from-end))

  (let ((filter (if key (compose pred key) (coerce pred 'function))))
    (declare (type function filter))
    (generating
      (if count
          (iter (for      element each sequence :from start :to end) ; TODO end
                (generate i :from 1)
                (while (or (not count) (< i count)))
                (unless (funcall filter element)
                  (next i)
                  (yield element)))

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

(defmethod sequence:substitute-if
    ((new t) (predicate t) (sequence traversal-sequence)
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

(defmethod sequence:replace
    ((sequence1 traversal-sequence) (sequence2)
     &key start1 end1 start2 end2)
  (with-traverser-at ((state next) sequence)
    (make-instance 'traversal-sequence
                   :state state
                   :next  #'next)))

#+no (defmethod sequence:merge ((sequence1 traversal-sequence))
  "TODO(jmoringe): document"
  )
(merge )
;;;

; TODO better name
(defclass traversal-sequence-with-limit (traversal-sequence)
  ((limit :initarg  :limit
          :type     non-negative-integer
          :reader   sequence-limit
          :documentation
          ""))
  (:default-initargs
   :limit (required-argument :limit))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod sequence:length ((sequence traversal-sequence-with-limit))
  (sequence-limit sequence))

(defmethod #+not-yet sequence: length> ((query    integer)
                                        (sequence traversal-sequence-with-limit))
  (> (sequence-limit sequence) query))

(defmethod sequence:make-sequence-iterator
    ((sequence traversal-sequence-with-limit)
     &key
     (start 0)
     end
     from-end)
  (declare (ignore start end from-end))
  (let+ (((&accessors-r/o (limit sequence-limit)) sequence)
         ((&values state end from-end next endp
                   element set-element index copy)
          (call-next-method))
         ((&flet iterator-endp (sequence state limit1 from-end)
            ; TODO slow
            (or (= (funcall index sequence state) limit)
                (funcall endp sequence state limit1 from-end)))))
    (values state end from-end
            next #'iterator-endp element set-element index copy)))

(defmethod sequence:subseq ((sequence traversal-sequence-with-limit) start &optional end)
  (let+ (((&accessors-r/o (limit sequence-limit)) sequence))
    (unless (<= 0 start limit)
      (error "~S ~D outside sequence bounds ~D ~D"
             :start start 0 limit))
    (unless (or (not end) (<= 0 end limit))
      (error "~S ~D outside sequence bounds ~D ~D"
             :end end 0 limit))

    (with-traverser-r/o ((state next) sequence)
      (iter (repeat start)
            (setf state (nth-value 1 (funcall next state))))
      (make-instance 'traversal-sequence-with-limit
                     :state state
                     :next  next
                     :limit (- (or end limit) start)))))

;;; Iterator

#+old (defmethod sequence:iterator-element ((sequence traversal-sequence)
                                      (iterator simple-array))
  (declare #+later (optimize (debug 0) (safety 0) (speed 3))
           (type (simple-array t (4)) iterator))
  (aref iterator 0))

#+old (defmethod sequence:iterator-step ((sequence traversal-sequence)
                                   (iterator simple-array)
                                   (from-end t))
  (declare #+later (optimize (debug 0) (safety 0) (speed 3))
           (type (simple-array t (4)) iterator)) ; TODO type
  (let+ ((#(&ign cont index end) iterator))
    (declare (type function cont)
             (type non-negative-integer index))
    (multiple-value-call #'vector (funcall cont) (1+ index) end)))

#+old (defmethod sequence:iterator-endp ((sequence traversal-sequence)
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
