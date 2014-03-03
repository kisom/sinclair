#|
  This file is a part of the sinclair project.
  Copyright (c) 2014 K. Isom (kyle@tyrfingr.is)
|#

(in-package :cl-user)
(defpackage sinclair
  (:use :cl))
(in-package :sinclair)

;; Hacks and glory await!

(defun koala-list-files (path)
  (let ((path (if (null path)
                  #P "."
                  path)))
      (read-from-string
       (string-right-trim *trailing-whitespace*
         (with-output-to-string (stream)
           (sb-ext:run-program *koala-path*
                               `("-o" "lisp" ,(namestring path))
                        :output stream))))))

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defvar *trailing-whitespace* '(#\Space #\Tab #\Newline))
(defvar *koala-path* #P"/home/kyle/code/go/bin/koala")
(defvar *md-extension* ".md")

(defun ends-with (filename extension)
  (let ((filename (if (pathnamep filename)
                      (namestring filename)
                      filename)))
    (let ((name-length (length filename)))
      (if (< name-length (+ (length extension) 1))
          nil
          (let ((start-pos
                 (- name-length
                    (length extension))))
            (if (equalp (subseq filename start-pos)
                        extension)
                t
                nil))))))
 
(defun render-files (path)
  (let ((file-list 
         (remove-if-not (lambda (filename)
                          (ends-with filename *md-extension*))
                        (koala-list-files path))))
    (mapcar (lambda (filename)
              (with-output-to-string (stream)
                (let ((body (file-string filename)))
                  (cl-markdown:markdown body
                                        :stream stream
                                        :format :html))))
            file-list)))
