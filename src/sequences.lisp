;;;; sequences.lisp --- Sequence sub-classes.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:utilities.sequence)

;; TODO consider graphs
;;
;; s1 -> map f s1 s2 =: s3 -> map g s3 s4
;;       ^                    ^
;;       |                    |
;; s2 ---'                    |
;;                            |
;; s4 ------------------------'
;;
;; =>
;;
;; map (lambda (a b c) (g (f a b) c)) s1 s2 s4

(defclass delayed-iterator-mixin ()
  ())

(defclass transforming-mixin ()
  ((sequences :initarg :sequences
              :type    list
              :reader  sequence-sequences))
  (:documentation
   "Intended to be mixed into sequence classes that compute their
    elements by transforming one or more input sequences."))

(defclass function-mixin ()
  ((function :initarg :function
             :type    function
             :reader  sequence-function))
  (:documentation
   "Intended to be mixed into sequence classes that compute their
    elements calling a function on the elements of one or more input
    sequences."))

(defclass map-sequence (sequence-proxy-mixin
                        delayed-iterator-mixin
                        transforming-mixin
                        function-mixin
                        sequence)
  ())

(defclass filter-sequence (sequence-proxy-mixin
                           function-mixin
                           sequence)
  ())
