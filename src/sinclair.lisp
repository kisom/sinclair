#|
  This file is a part of the sinclair project.
  Copyright (c) 2014 K. Isom (kyle@tyrfingr.is)
|#

(in-package :cl-user)
(restas:define-module #:sinclair
  (:use #:cl #:restas #:sexml))

(sexml:with-compiletime-active-layers
    (sexml:standard-sexml sexml:xml-doctype)
  (sexml:support-dtd
   (merge-pathnames "html5.dtd" (asdf:system-source-directory "sexml"))
   :<))
(<:augment-with-doctype "html" "")

(in-package :sinclair)

;; Hacks and glory await!

(defvar *trailing-whitespace* '(#\Space #\Tab #\Newline))
(defvar *koala-path* #P"/home/kyle/go/bin/koala")
(defvar *dibbler-path* #P"/home/kyle/go/bin/dibbler")
(defvar *md-extension* ".md")
(defvar *sinclair-root* #P "/home/kyle/sites/sinclair")
(defvar *pretty-date-format* '((:year 4 ) "-" (:month 2) "-" (:day 2)))

(defvar *node-store* (make-hash-table :test #'equal))
(defvar *asset-store* '())
(defvar *site-config* (make-hash-table))

;;; utilities

(defun to-keyword (sym)
  (intern (string sym) "KEYWORD"))

(defun toggle-swank (&key (port 4006))
  "Setup a swank server in the running process."
  (let ((swank-runningp nil))
    (labels ((fn ()
                 (progn
                   (if swank-runningp
                       (progn
                         (format t "Stopping swank server...~%")
                         (swank:stop-server port)
                         (setf swank-runningp nil))
                       (progn
                         (format t "Starting swank server...~%")
                         (swank:create-server :port port :dont-close t)
                         (setf swank-runningp t)))
                   swank-runningp)))
      (setf (symbol-function 'setup-swank) #'fn)
      (setup-swank))))

;;; start up actions

(defun startup (&key (mode :dev) (port 8080))
  (when (equal mode :prod)
    (progn
      (restas:debug-mode-off)
      (toggle-swank))
    (progn
      (restas:debug-mode-on)))
  (update-header (default-header))
  (reload-site :from-disk t)
  (restas:start '#:sinclair :port port))

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

(defun pretty-node-date (node)
  (local-time:format-timestring nil (node-date node)
                                :format *pretty-date-format*))

(defun load-node (path)
  "Load the node from the store."
  (multiple-value-bind (node present) (gethash path *node-store*)
    (when present
      node)))

(defun store-node (node)
  "Store the node in the store"
  (setf (gethash (node-path node) *node-store*) node))

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

(defun node-in-store (path)
  "Predicate determining whether path is a valid node in the store."
  (nth-value 1 (getf path *node-store*)))

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
        (not (node-in-store path))
        (let ((node (load-node path)))
          (> mtime 
             (local-time:timestamp-to-unix (node-mtime node)))))))

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

(defun strip-prefix (s prefix)
  (if (zerop (search prefix s))
      (subseq s (length prefix))
      s))

(defun static-slug (node)
  (let ((root-path (namestring *sinclair-root*)))
      (if (zerop
         (search root-path (node-path node)))
          (let ((slug (subseq (node-path node)
                              (length root-path))))
            (if (equal (pathname-name slug) "index")
                (format nil "/~{~A/~}" (cdr (pathname-directory slug)))
                slug))
          nil)))

(defun filter-pages (node-list)
  "Filter out pages from the list of nodes."
  (remove-if (lambda (node)
               (equal (node-mode node) :page))
             node-list))

(defun unique-list (lst)
  "Given a sort list, returns the list as a set."
  (labels ((skip (skip-lst)
             (if (equal (first skip-lst)
                        (second skip-lst))
                 (skip (cdr skip-lst))
                 (if (null skip-lst)
                    nil
                    (cdr skip-lst))))
           (collect (skip-lst)
             (if (null skip-lst)
                 nil
                 (cons (car skip-lst) (collect (skip skip-lst))))))
    (collect (copy-tree lst))))

(defun load-all-nodes ()
  (mapcar #'load-node
          (loop for key being the hash-keys of *node-store* collecting key)))

(defun sort-nodes-by-time (node-list)
  "Sort the list of nodes in descending order by time."
  (labels ((node-year (node)
             (local-time:timestamp-to-unix
              (node-date node))))
    (sort node-list
          (lambda (x y)
            (> (node-year x)
               (node-year y))))))

(defun group-nodes-by-year (node-list)
  "Group nodes by their year in descending order."
  (let ((year-list (sort
                    (mapcar (compose #'local-time:timestamp-year
                                     #'node-date)
                            node-list)
                    #'>)))
    (labels ((filter-by-year (year)
               (sort-nodes-by-time
                (remove-if (compose #'not
                                    (lambda (node-year) (equal node-year year))
                                    #'local-time:timestamp-year
                                    #'node-date)
                           node-list))))
      (mapcar (lambda (year)
                (list :year year
                      :nodes (filter-by-year year)))
              (unique-list year-list)))))

(defun index-for-year (nodes)
  (let ((year (format nil "~A" (getf nodes :year))))
    (<:div :class "year-group"
           (<:h3 year)
           (<:ul
            (mapcar (lambda (node)
                        (<:li (<:a :href (build-slug node)
                                       (format nil "~A: ~A"
                                                (pretty-node-date node)
                                                (node-title node)))))
                      (getf nodes :nodes))))))

(defun update-header (&rest forms)
  (setf (gethash :header *site-config*)
        (<:div :id "header" forms)))

(defun load-header ()
  (let ((header (gethash :header *site-config*)))
    (if header
        header
        (<:div :id "header"))))

(defun load-stylesheets ()
  (let ((styles (gethash :styles *site-config*)))
    (if styles
        styles
        '())))

(defun intern-string (s)
  (intern (string-upcase s)))

(defun sanitize-static-slug (slug)
  (map 'string
       (lambda (c)
         (if (or (equal c #\/)
                 (equal c #\.))
             #\-
             c))
       slug))

(defun default-header ()
  (list
   (<:h2 "metacircular")
   (<:div :id "root-link" (<:a :href "/" "Return to top-level"))
   (<:ul
    (<:li (<:a :href "/about/" "About"))
    (<:li (<:a :href "http://kyleisom.net/" "Homepage"))
    (<:li (<:a :href "https://twitter.com/kyleisom" "Twitter"))
    (<:li (<:a :href "https://github.com/kisom/" "Github"))
    (<:li (<:a :href "https://bitbucket.org/kisom/" "Bitbucket")))))
