#|
  This file is a part of sinclair project.
  Copyright (c) 2014 K. Isom (kyle@tyrfingr.is)
|#

#|
  Simple blog engine

  Author: K. Isom (kyle@tyrfingr.is)
|#

(in-package :cl-user)
(defpackage sinclair-asd
  (:use :cl :asdf))
(in-package :sinclair-asd)

(defsystem sinclair
  :version "0.1"
  :author "K. Isom"
  :license "ISC"
  :depends-on (:cl-markdown
               :cl-redis
               :cl-who
               :restas
               :st-json
               :swank)
  :components ((:module "src"
                :components
                ((:file "sinclair"))))
  :description "Simple blog engine"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op sinclair-test))))
