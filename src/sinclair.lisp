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


;;; start up actions

(defun startup ()
  (key-store-setup))

(defun key-store-setup ()
  (redis:connect))

;;; node scanning


(defun koala-list-files (path)
  "Dispatch koala forth to retrieve a list of files at the path, returned as a list of pathnames."
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
  "Read an entire file into a string buffer."
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun ends-with (filename extension)
  "Predicate that returns true if the filename ends with the extension."
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
  "Dispatch dibbler to proceess the files listed. Returns an ST-JSON structure with the results."
  (let ((paths (if (listp paths)
                   paths
                   (list paths))))
    (let ((paths (mapcar #'namestring paths)))
      (st-json:read-json-from-string
       (string-right-trim *trailing-whitespace*
                          (with-output-to-string (stream)
                            (sb-ext:run-program *dibbler-path*
                                                paths
                                                :output stream)))))))

(defun filter-nodes (paths)
  "Remove any paths not ending in the markdown extension."
  (remove-if-not (lambda (filename)
                   (ends-with filename *md-extension*))
                 paths))

(defun filter-out-nodes (paths)
  "Remove any paths ending in the markdown extension."
  (remove-if (lambda (filename)
               (ends-with filename *md-extension*))
             paths))

(defun push-failed (not-nodes node-json)
  "Push any of the failed nodes from node-json into the collection of not-nodes."
  (let ((failed-node-list (remove-if (lambda (node)
                                       (equalp :TRUE
                                               (st-json:getjso "success"
                                                               node)))
                                     node-json))
        (not-nodes not-nodes))

    (dolist (failed-node failed-node-list)
      (push (st-json:getjso "path" failed-node) not-nodes))
    not-nodes))


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
  "Given a node JSON structure as returned by the dibbler minion in its default mode, retrieves the key from the node."
  (multiple-value-bind (value present)
      (st-json:getjso key (st-json:getjso "node" node))
    (if present
        value nil)))

(defun make-node (node)
  "Given a JSON representation of a node result container, process it into a node object."
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

(defun red-node-slot (node-name slot-name)
  "Retrieve the slot value from redis for the given node."
  (multiple-value-bind (value present)
      (redis:red-hget node-name slot-name)
    (if present
        value
        nil)))

(defun load-node (path)
  "Load the node from Redis."
  (let ((node-name (concatenate 'string "node-" path)))
    (if (not (node-in-redis path))
        nil
        (make-instance 'node
                       :mtime (red-node-slot node-name "mtime")
                       :path  (red-node-slot node-name "path")
                       :slug  (red-node-slot node-name "slug")
                       :body  (red-node-slot node-name "body")
                       :tags  (red-node-slot node-name "tags")
                       :mode  (red-node-slot node-name "mode")
                       :date  (red-node-slot node-name "date")
                       :title (red-node-slot node-name "title")))))

(defun send-forth-minions (paths)
  "Send forth the minions who will seek out whom they may devour."
  (let* ((file-list (koala-list-files paths))
         (not-nodes (filter-out-nodes file-list))
         (node-list (filter-nodes file-list))
         (node-json (dibbler-load-nodes node-list)))
    (let ((not-nodes (push-failed not-nodes node-json))
          (node-json (remove-if (lambda (node)
                                  (equalp :FALSE
                                          (st-json:getjso "success" node)))
                                node-json)))
      (list
       :FAILED not-nodes
       :NODES (mapcar #'make-node node-json)))))

(defun node-in-redis (path)
  "Predicate determining whether path is a valid node in redis."
  (let ((node-name (concatenate 'string "node-" path)))
    (redis:red-exists node-name)))

(defun get-mtime (path)
  "Retrieve the mtime for the path from disk, using dibbler."
  (let ((mod-json
         (first
          (st-json:read-json-from-string
           (string-right-trim *trailing-whitespace*
                              (with-output-to-string (stream)
                                (sb-ext:run-program *dibbler-path*
                                                    (list "-mod" (namestring path))
                                                    :output stream)))))))
    (if (equalp (st-json:getjso "success" mod-json) :FALSE)
        nil
        (st-json:getjso "mtime"
                        (st-json:getjso "result"
                                        mod-json)))))


(defun node-should-update (path)
  "Predicate returning T if a node should update itself."
  (let ((mtime (get-mtime path)))
    (or (null mtime)
        (not (node-in-redis path))
        (let ((node (load-node path)))
          (> mtime (node-mtime node))))))
