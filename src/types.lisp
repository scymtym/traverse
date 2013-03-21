;;;; types.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:traverse)

(deftype traverser ()
  "A traverser is a thunk which returns two values: the current value
   and a traverser for the rest of the structure."
  '(function () (values t function)))
