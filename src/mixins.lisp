;;;; mixins.lisp --- Sequence mixins.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:utilities.sequence)

;; mixin for state if index and limit are integer; can provide fast length
;; remember length when subseq is called with end

;;; iterator tools

(declaim (inline advance make-endp))

(defun advance (sequence iterator limit step endp amount)
  (iter (repeat amount) ; TODO check endp?
        (setf iterator (funcall step sequence iterator limit))
        (finally (return iterator))))

(defun make-endp (endp index end)
  (named-lambda truncated-endp (sequence iterator limit from-end)
    (or (funcall endp sequence iterator limit from-end)
        (>= (funcall index sequence iterator) end))))

;;; sequence-proxy-mixin

(deftype iterator-data ()
  `(simple-array t (9))) ; TODO define a structure

(defclass sequence-proxy-mixin ()
  ((iterator :initarg  :iterator
             :type     iterator-data
             :accessor sequence-iterator
             :documentation
             "TODO"))
  (:default-initargs
   :iterator (required-argument :iterator))
  (:documentation
   "TODO(): document"))

(defmethod print-object ((object sequence-proxy-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((length (apply #'min (or *print-length* 5)
                         (unless (length> 5 object)
                           (list (length object))))))
      (format stream "~@<~:[<empty>~;~:*~{~A~^ ~}~]~:[~; ...~]~@:>"
              (when (plusp length)
                (coerce (subseq object 0 length) 'list))
              (length> length object)))))

;; TODO class-allocated slot?
(defclass empty-sequence-proxy-mixin-sequence (sequence-proxy-mixin
                                               sequence)
  ()
  (:default-initargs
   :iterator (vector nil nil 0
                     (lambda (&rest args) (declare (ignore args)) (error "cannot step"))
                     (constantly t)
                     (lambda (&rest args) (declare (ignore args)) (error "elt error"))
                     (lambda (&rest args) (declare (ignore args)) (error "(setf elt) error"))
                     (constantly 0)
                     (constantly nil))))

(defvar #+no define-constant +the-empty-sequence-proxy-mixin-sequence+
    (make-instance 'empty-sequence-proxy-mixin-sequence))

(defmacro with-proxy-sequence-iterator ((sequence iterator limit &rest vars) instance
                                        &body body)
  (let+ (((&with-gensyms data))
         ((&flet make-binding (var index)
            (when var
              `((,var (svref ,data ,index))))))) ; TODO make a structure or avoid bounds-checks?
    `(let* ((,data (slot-value ,instance 'iterator))
            ,@(mappend #'make-binding
                       (list* sequence iterator limit vars)
                       (iota (+ 3 (length vars)))))
       (declare (type iterator-data ,data)
                (type function ,@(remove nil vars)))
       ,@body)))

#+no (defmacro with-traverser-at (((state-var next-var &key start) sequence) &body body)
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
    ((sequence sequence-proxy-mixin)
     (length   t)
     &key
     (initial-element nil initial-element-supplied-p)
     (initial-contents nil initial-contents-supplied-p)
     &allow-other-keys)
  (let ((iterator
          (cond
            ((and (not initial-element-supplied-p)
                  (not initial-contents-supplied-p))
             (multiple-value-call #'vector
               sequence
               (multiple-value-bind (iterator limit from-end step endp elt setelt index copy) ;TODO
                   (sequence:make-sequence-iterator sequence :end length)
                 (declare (ignore from-end))
                 (values iterator limit step endp elt setelt index copy))))
            (initial-element-supplied-p
             (values
              nil
              (labels ((constantly2 (state)
                         (values initial-element state)))
                #'constantly2)))
            (initial-contents-supplied-p
             (with-yield ;; TODO make a function
               (iter (for element each initial-contents)
                     (yield element))))
            (t
             (error "~@<~S and ~S are mutually exclusive.~@:>"
                    :initial-element :initial-contents)))))
    (make-instance (class-of sequence) :iterator iterator))
  #+no (let+ (((&values state next)
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

(defmethod sequence:emptyp ((sequence sequence-proxy-mixin))
  (with-proxy-sequence-iterator (sequence iterator limit nil endp) sequence
    (funcall endp sequence iterator limit)))

(defmethod sequence:length ((sequence sequence-proxy-mixin))
  (with-proxy-sequence-iterator (sequence iterator limit step endp nil nil nil copy) sequence
    (let ((iterator (print (funcall copy sequence iterator))))
      (print (list iterator limit endp))
      (iter (until (print (funcall endp sequence iterator limit nil)))
            (setf iterator (funcall step sequence iterator limit))
            (counting 1)))))

(defmethod #+not-yet sequence: length> ((query    integer)
                                        (sequence sequence-proxy-mixin))
  (with-proxy-sequence-iterator (sequence iterator limit step endp nil nil nil copy) sequence
    (cond
      ((funcall endp sequence iterator limit nil)
       (minusp query))
      ((zerop query)
       nil)
      (t
       (let ((iterator (funcall copy sequence iterator)))
         (iter (until (funcall endp sequence iterator limit nil))
               (setf iterator (funcall step sequence iterator limit))
               (for i :from 0)
               (when (= i query)
                 (return t))))))))

(defmethod sequence:elt ((sequence sequence-proxy-mixin)
                         (index    integer))
  ;; TODO with-proxy-sequence-iterator-at?
  (with-proxy-sequence-iterator (sequence iterator limit step endp elt nil nil copy) sequence
    (let ((iterator (funcall copy sequence iterator))) ; TODO macro
      (setf iterator (advance sequence iterator limit step endp index))
      (funcall elt sequence iterator))))

(defmethod (setf sequence:elt) ((new-value t)
                                (sequence  sequence-proxy-mixin)
                                (index     integer))
  ;; TODO change-class of SEQUENCE to
  ;; element-replacement-sequence-proxy-mixin and record the replaced
  ;; element?
  (error 'simple-program-error "~@<Sequence ~A is immutable.~@:>" sequence))

(defmethod sequence:make-sequence-iterator ((sequence sequence-proxy-mixin)
                                            &key
                                            start
                                            end
                                            from-end)
  (when from-end
    (error "~S is not supported" :from-end))

  (with-proxy-sequence-iterator (sequence iterator limit step endp elt setelt index copy) sequence
    (let* ((iterator (funcall copy sequence iterator))
           (iterator (if (and start (plusp start))
                         (advance sequence iterator limit step endp start)
                         iterator))
           (endp     (if end (make-endp endp index end) endp)))
      (values iterator limit from-end step endp elt setelt index copy))))
