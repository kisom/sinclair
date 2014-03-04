#|
  This file is a part of the sinclair project.
  Copyright (c) 2014 K. Isom (kyle@tyrfingr.is)
|#

(in-package :cl-user)
(defpackage sinclair
  (:use :cl))
(in-package :sinclair)

;; Hacks and glory await!

(defvar *trailing-whitespace* '(#\Space #\Tab #\Newline))
(defvar *koala-path* #P"/home/kyle/code/go/bin/koala")
(defvar *dibbler-path* #P"/home/kyle/code/go/bin/dibbler")
(defvar *md-extension* ".md")


(defun koala-list-files (path)
  (let ((path (if (null path)
                  #P "."
                  path)))
      (read-from-string
       (string-right-trim *trailing-whitespace*
         (with-output-to-string (stream)
           (sb-ext:run-program *koala-path*
                               `("-s" "-o" "lisp" ,(namestring path))
                        :output stream))))))

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

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

(defun dibbler-load-nodes (paths)
  (let ((paths (if (listp paths)
                   paths
                   (list paths))))
    (let ((paths (mapcar #'namestring paths)))
      (string-right-trim *trailing-whitespace*
                         (with-output-to-string (stream)
                           (sb-ext:run-program *dibbler-path*
                                               paths
                                               :output stream))))))

(defun path-to-arg-string (path)
  (if (pathnamep path)
      (namestring path)
      path))

(defun dispatch-dibbler (path)
  (let ((path (path-to-arg-string path)))
      (string-right-trim *trailing-whitespace*
                         (with-output-to-string (stream)
                           (sb-ext:run-program *dibbler-path*
                                               (list path)
                                               :output stream)))))

(defun filter-nodes (paths)
  (remove-if-not (lambda (filename)
                   (ends-with filename *md-extension*))
                 paths))



;;; A node can be either a page or a post. A page is a static page
;;; that does not have a date, author, or set of tags associated with
;;; it. A post is written by an author at a specific time, and has
;;; some tags associated with that allow the post to be grouped
;;; together by related subjects. Index-wise, posts are indexed on the
;;; front page., organised by year. It is assumed the template will
;;; contain links to pages, or at least to starting points. Pages are
;;; not automatically indexed on the front page. Finally, pages are
;;; excluded from RSS updates.

(defclass node ()
  ((title :initarg :title
          :accessor node-title
          :documentation "node's title used in HTML rendering")
   (date  :initarg :date
          :accessor node-date
          :documentation "date the node was published")
   (mode  :initarg :mode
          :accessor node-mode
          :documentation "the post's mode; one of :post or :page")
   (tags  :initarg :tags
          :accessor node-tags
          :type 'list
          :documentation
          "a list of strings containing metadata associated with the node")
   (body  :initarg :body
          :accessor node-body
          :documentation "the rendered HTML fragment of the body")
   (slug  :initarg :slug
          :accessor node-slug
          :documentation "the relative URL of the node")
   (path  :initarg :path
          :accessor node-path
          :documentation "the filesystem path to the underlying file")
   (mtime :initarg :mtime
          :accessor node-mtime
          :documentation "last-modified time used for updates"))
  (:documentation "the node class contains information representing a node"))

(defun key-from-node (key node)
  (multiple-value-bind (value present)
      (st-json:getjso key (st-json:getjso "node" node))
    (if present
        value nil)))

(defun make-node (node)
  (let ((success (st-json:from-json-bool
                  (st-json:getjso "success" node))))
    (if (not success)
        nil
        (make-instance 'node
                       :mtime (key-from-node "mtime" node)
                       :path (key-from-node "path" node)
                       :slug (key-from-node "slug" node)
                       :body (key-from-node "body" node)
                       :tags (key-from-node "tags" node)
                       :mode (if (key-from-node "static" node) :page :post)
                       :date (key-from-node "date" node)
                       :title (key-from-node "title" node)))))
