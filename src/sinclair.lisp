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
(defvar *sinclair-root* #P "/home/kyle/tmp/sinclair")


;;; utilities
(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))


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

(defun unix-to-timestamp (timestamp)
  (local-time:unix-to-timestamp
   (if (numberp timestamp)
       timestamp
       (parse-integer timestamp))))

(defun make-node (node)
  "Given a JSON representation of a node result container, process it into a node object."
  (let ((success (st-json:from-json-bool
                  (st-json:getjso "success" node))))
    (if (not success)
        nil
        (make-instance 'node
                       :mtime (unix-to-timestamp
                               (key-from-node "mtime" node))
                       :path (key-from-node "path" node)
                       :slug (key-from-node "slug" node)
                       :body (key-from-node "body" node)
                       :tags (key-from-node "tags" node)
                       :mode (if (equalp 
                                  (key-from-node "static" node)
                                  :TRUE) :page :post)
                       :date (unix-to-timestamp
                              (key-from-node "date" node))
                       :title (key-from-node "title" node)))))

(defun red-node-slot (node-name slot-name)
  "Retrieve the slot value from redis for the given node."
  (redis:red-hget node-name slot-name))

(defun tags-as-keywords (tags)
  "Convert a list of tag symbols to keywords"
  (mapcar (lambda (s)
            (intern s "KEYWORD"))
          (mapcar #'string tags)))

(defun tags-as-strings (tags)
  "Convert a list of tag keywords to strings"
  (mapcar #'string-downcase
          (mapcar #'string
                  tags)))

(defun redis-name (path)
  (concatenate 'string
               "node-"
               path))

(defun load-node (path)
  "Load the node from Redis."
  (load-node-by-name (redis-name path)))

(defun load-node-by-name (node-name)
  (if (not (redis:red-exists node-name))
      nil
      (make-instance 'node
                     :mtime (unix-to-timestamp
                             (red-node-slot node-name "mtime"))
                     :path  (red-node-slot node-name "path")
                     :slug  (red-node-slot node-name "slug")
                     :body  (red-node-slot node-name "body")
                     :tags  (let ((tags (red-node-slot node-name "tags")))
                              (if (null tags)
                                  nil
                                  (tags-as-keywords
                                   (read-from-string tags))))
                     :mode  (intern (red-node-slot node-name "mode")
                                    "KEYWORD")
                     :date  (unix-to-timestamp
                             (red-node-slot node-name "date"))
                     :title (red-node-slot node-name "title"))))

(defun store-node-slot (node slot)
  (redis:red-hset (redis-name (node-path node))
                  (string-downcase (string slot))
                  (slot-value node slot)))

(defun store-node-timestamp (node slot)
  (redis:red-hset (redis-name (node-path node))
                  (string-downcase (string slot))
                  (local-time:timestamp-to-unix
                   (slot-value node slot))))

(defun store-node (node)
  "Store the node in Redis."
  (progn
    (store-node-slot node 'title)
    (store-node-slot node 'path)
    (store-node-slot node 'slug)
    (store-node-slot node 'body)
    (store-node-slot node 'tags)
    (store-node-slot node 'mode)
    (store-node-timestamp node 'date)
    (store-node-timestamp node 'mtime)))

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
       :ASSETS not-nodes
       :NODES (mapcar #'make-node node-json)))))

(defun node-in-redis (path)
  "Predicate determining whether path is a valid node in redis."
  (redis:red-exists (redis-name path)))

(defun get-mtime (path)
  "Retrieve the mtime for the path from disk, using dibbler."
  (let ((mod-json
         (first
          (st-json:read-json-from-string
           (string-right-trim *trailing-whitespace*
                              (with-output-to-string (stream)
                                (sb-ext:run-program *dibbler-path*
                                                    (list "-mod" 
                                                          (namestring path))
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
          (> mtime (parse-integer (node-mtime node)))))))


(defun sort-by-year (node-list)
  (sort (copy-list node-list)
        (lambda (node-1 node-2)
          (< (local-time:timestamp-year
              (node-date node-1))
             (local-time:timestamp-year
              (node-date node-2))))))

(defun build-slug (node)
  (if (equalp (node-mode node) :page)
      (static-slug node)
      (format nil "/blog/~A/~A/~A/~A"
              (local-time:timestamp-year (node-date node))
              (local-time:timestamp-month (node-date node))
              (local-time:timestamp-day (node-date node))
              (node-slug node))))

(defun static-slug (node)
  (let ((root-path (namestring *sinclair-root*)))
      (if (zerop
           (search root-path (node-path node)))
          (subseq (node-path node-1) (length root-path))
          nil)))

