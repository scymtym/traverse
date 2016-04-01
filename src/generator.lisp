;;;; generator.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:utilities.sequence)

;;; Actual generator macros and functions

;; Signal an error if `yield' is used outside the lexical scope of
;; `with-yield'.
(defmacro yield (value)
  (cerror "continue" "~@<~S used outside lexical scope of ~S.~@:>"
          'yield 'with-yield)
  `(yield1 ,value))

(defmacro with-yield (&body body)
  `(values
    (lambda ()
      (cont:with-call/cc
        (macrolet ((yield (value)
                     `(cont:let/cc k
                        (values ,value k))))
          ,@body
          (values nil nil))))
    #'funcall))

(defmacro generating (&body body)
  `(with-yield ,@body))

(let+ (((&values state1 next) ((lambda () (with-yield (iter (for i :from 0) (yield i))))))
       (state state1))
  (let+ (((&values value state*) (funcall next state)))
    (print value)
    (setf state state*))
  (let+ (((&values value state*) (funcall next state)))
    (print value)
    (setf state state*))
  (let+ (((&values value state*) (funcall next state)))
    (print value)
    (setf state state*))
  (let+ (((&values value state*) (funcall next state)))
    (print value)
    (setf state state*))

  (setf state state1)
  (let+ (((&values value state*) (funcall next state)))
    (print value)
    (setf state state*))
  (let+ (((&values value state*) (funcall next state)))
    (print value)
    (setf state state*))
  (let+ (((&values value state*) (funcall next state)))
    (print value)
    (setf state state*))
  (let+ (((&values value state*) (funcall next state)))
    (print value)
    (setf state state*)))

;;; Generator sequence

(defstruct (iterator-state
             (:constructor make-iterator-state
                           (state start &optional element (initial state))))
  (state   (required-argument :state)   :type (or null function))
  (start   (required-argument :start)   :type non-negative-integer)
  (element nil                          :type t)
  (initial (required-argument :initial) :type t                    :read-only t))

(declaim (ftype (function (function non-negative-integer)
                          (values t function &optional))
                generator->iterator-advance)
         (inline generator->iterator-advance))
(defun generator->iterator-advance (state amount)
  (let ((state state))
    (iter (repeat amount)
          (setf state (the function (nth-value 1 (funcall state)))) ; TODO or null?
          (unless state
            (error "premature end of sequence")))
    (funcall state)))

(declaim (ftype (function (t iterator-state t)
                          (values iterator-state &optional))
                generator->iterator-next))
(defun generator->iterator-next (sequence iterator limit)
  (declare (ignore sequence limit))
  (with-accessors ((state   iterator-state-state) ; TODO structure-r/o
                   (start   iterator-state-start)
                   (element iterator-state-element))
      iterator
    (incf start)
    (setf (values element state) (funcall state))) ; TODO can be faster if we know NEXT == FUNCALL
  iterator)

(declaim (ftype (function (t iterator-state t t) (values t &optional))
                generator->iterator-endp))
(defun generator->iterator-endp (sequence iterator limit from-end)
  (declare (ignore sequence limit from-end))
  (null (iterator-state-state iterator)))

(declaim (ftype (function (t iterator-state) (values t &optional))
                generator->iterator-elt))
(defun generator->iterator-elt (sequence iterator)
  (declare (ignore sequence))
  (iterator-state-element iterator))

(defun (setf generator->iterator-elt) (new-value sequence iterator) ; TODO make sure these don't cons
  (declare (ignore new-value iterator))
  (error "Sequence ~A is immutable."
         sequence))

(declaim (ftype (function (t iterator-state)
                          (values non-negative-integer &optional))
                generator->iterator-index))
(defun generator->iterator-index (sequence iterator)
  (declare (ignore sequence))
  (iterator-state-start iterator))

(declaim (ftype (function (t iterator-state)
                          (values iterator-state &optional))
                generator->iterator-copy/advance-initial))
(defun generator->iterator-copy/advance-initial (sequence iterator)
  (declare (ignore sequence))
  (let+ ((start   (iterator-state-start iterator))
         (initial (iterator-state-initial iterator))
         ((&values element state) (generator->iterator-advance initial start)))
    (make-iterator-state state start element initial)))

(defun generator->iterator (state &key (start 0))
  (let+ ((initial state)
         ((&values element state) (generator->iterator-advance initial start))
         (iterator (make-iterator-state state start element initial)))
    ;; Return iterator state with reader and mutator closures.
    (values nil iterator nil
            #'generator->iterator-next #'generator->iterator-endp
            #'generator->iterator-elt #'(setf generator->iterator-elt)
            #'generator->iterator-index #'generator->iterator-copy/advance-initial)))

(defclass generator-sequence (sequence-proxy-mixin
                              sequence)
  ())

(declaim (inline make-generator-sequence))
(defun make-generator-sequence (state next)
  (make-instance 'generator-sequence
                 :iterator (multiple-value-call #'vector
                             (generator->iterator state))))

(declaim (ftype (function (function) (values generator-sequence &optional))))
(defun call-with-generating-sequence (thunk)
  (multiple-value-call #'make-generator-sequence (funcall thunk)))

(defmacro generating-sequence (&body body)
  `(call-with-generating-sequence (lambda () (with-yield ,@body))))
