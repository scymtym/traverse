;;;; types.lisp --- Types used by the traverse system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:utilities.sequence)

(deftype generator-function ()
  "A traverser is a function which, given a state, returns two values:
   the current value and a new state for traversing the rest of the
   structure."
  '(function (t) (values t t)))
