;;;; traverse.asd --- System definition for traverse system.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:traverse-system
  (:use
   #:cl
   #:asdf))


(cl:in-package #:traverse-system)

(defsystem :traverse
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
                 :components ((:file       "package")

                              (:file       "types"
                               :depends-on ("package"))
                              (:file       "protocol"
                               :depends-on ("package"))

                              (:file       "builtins"
                               :depends-on ("package" "protocol"))))))

(defsytem :traverse-test
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
