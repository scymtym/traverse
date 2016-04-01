;;;; utilities.sequence.asd --- System definition for utilities.sequence system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:utilities.sequence-system
  (:use
   #:cl
   #:asdf))

(cl:in-package #:utilities.sequence-system)

(defsystem :utilities.sequence
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "LLGPLv3; see COPYING file for details."
  :description "Interfaces for traversing arbitrary structures."
  :depends-on  (:alexandria
                (:version :let-plus "0.2")
                :iterate
                :cl-cont)
  :components  ((:module     "src"
                 :serial     t
                 :components ((:file       "package")
                              #+no (:file       "protocol")
                              (:file       "generator")
                              (:file       "mixins")
                              (:file       "sequences")
                              (:file       "operations")

                              #+later (:file       "builtins")))))

(defsystem :utilities.sequence-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "LLGPLv3; see COPYING file for details."
  :description "Interfaces for traversing arbitrary structures."
  :depends-on  (:alexandria
                (:version :let-plus "0.2")
                :eos)
  :components  ((:module     "test"
                 :components ((:file       "package")

                              ))))
