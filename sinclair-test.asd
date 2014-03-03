#|
  This file is a part of sinclair project.
  Copyright (c) 2014 K. Isom (kyle@tyrfingr.is)
|#

(in-package :cl-user)
(defpackage sinclair-test-asd
  (:use :cl :asdf))
(in-package :sinclair-test-asd)

(defsystem sinclair-test
  :author "K. Isom"
  :license "ISC"
  :depends-on (:sinclair
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:test-file "sinclair"))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)
                    (asdf:clear-system c)))
