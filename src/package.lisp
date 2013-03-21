;;;; package.lisp --- Package definition for the traverse system.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:traverse
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate)

  ;; Client protocol
  (:export
   #:map-structure)

  ;; Traversal protocol
  (:export
   #:make-traverser/stateful
   #:make-traverser/pure)

  ;; Simple traversal protocol
  (:export
   #:successors)

  (:documentation
   ""))
