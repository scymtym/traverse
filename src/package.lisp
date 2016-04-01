;;;; package.lisp --- Package definition for the traverse system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:utilities.sequence
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate)

  (:shadow
   #:compose)

  (:documentation
   ""))
