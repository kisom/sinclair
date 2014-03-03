#|
  This file is a part of sinclair project.
  Copyright (c) 2014 K. Isom (kyle@tyrfingr.is)
|#

(in-package :cl-user)
(defpackage sinclair-test
  (:use :cl
        :sinclair
        :cl-test-more))
(in-package :sinclair-test)

;; NOTE: To run this test file, execute `(asdf:test-system :sinclair)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
