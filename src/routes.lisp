(in-package :sinclair)

;;; page generation macros and routes

;;; load-and-index-nodes-by-year produces an HTML fragment, as a
;;; string, that indexes all the currently-loaded nodes by year,
;;; ordering each post in ascending order by date published grouped by
;;; year.
(defmacro load-and-index-nodes-by-year ()
  `(mapcar #'index-for-year
           (group-nodes-by-year
            (filter-pages
             (load-all-nodes)))))

;;; invalidate-route causes the named route and URL to be redirected
;;; back to the index page.
(defmacro invalidate-route (route-name route-url)
  `(restas:define-route ,route-name (,route-url)
     (restas:redirect 'index-route)))

;;; logger defines the endpoint to send logs to, and provides this
;;; site's name for annotation in the logs.
(defparameter logger (logsling:new-logger "127.0.0.1:4141" "metacircular.net"))

;;; sling-logs sends the log, marked with the provided status, to the
;;; log collector.
(defun sling-log (status)
  (logsling:sling logger status hunchentoot:*request*))

;;; The index route shows the list of posts, grouped by year in
;;; ascending date order (i.e. generated from
;;; `load-and-index-nodes-by-year`).
(restas:define-route index-route ("/" :method :get)
  (progn
    ()
    (sling-log 200)
    (concatenate 'string
                 (<:doctype)
                 (<:html
                  (<:head
                   (<:title (or (gethash :title *site-config*) ""))
                   (<:meta :charset "UTF-8")
                   (mapcar (lambda (style)
                             (<:link :type "text/css"
                                     :rel "stylesheet"
                                     :href style))
                           (load-stylesheets)))
                  (<:body
                   (<:div :id "container"
                          (load-header)
                          (<:div :id "content"
                                 (<:h2 "Table of Contents")
                                 (load-and-index-nodes-by-year))))))))

;;; The tags-route displays a sorted, unordered list of all the tags
;;; in all posts. These are linked to tag pages, which display a list
;;; of all the posts that are thusly tagged.
(restas:define-route tags-route ("/tags" :method :get)
 (concatenate 'string
               (<:doctype)
               (<:html
                (<:head
                 (<:title (or (gethash :title *site-config*) ""))
                 (<:meta :charset "UTF-8")
                 (mapcar (lambda (style)
                           (<:link :type "text/css"
                                   :rel "stylesheet"
                                   :href style))
                         (load-stylesheets)))
                (<:body
                 (<:div :id "container"
                        (load-header)
                        (<:div :id "content"
                               (<:h2 "Tags")
                               (<:ul
                                (mapcar
                                 (lambda (tag)
                                   (<:li
                                    (<:a :href
                                         (format nil "/tag/~A" tag)
                                         tag)))
                                 (build-tags)))))))))

;;; tags-route2, tags-route3, and tags-route4 provide alternate tag
;;; page routes. They rely on the fact that `restas:define-route`
;;; generates a function with the route name that generates the HTML.
(restas:define-route tags-route2 ("/tags/" :method :get)
  (tags-route))

(restas:define-route tags-route3 ("/tag" :method :get)
  (tags-route))

(restas:define-route tags-route4 ("/tag/" :method :get)
  (tags-route))

;;; blog-rss generates a route for the RSS feed.
(restas:define-route blog-rss ("/index.rss" :method :get)
  (progn
    (sling-log 200)
    (rss-feed)))

;;; build-route generates a new route for the node, handling pages and
;;; posts differently.
(defun build-route (node)
  (when node
    (case (node-mode node)
      (:page (build-page-route node))
      (:post (build-post-route node))
      (otherwise nil))))

;;; build-post-route creates an appropriate route definition for a
;;; post; namely, it generates a slug that includes the base route
;;; "/blog" and a URL based on the date and slug.
(defun build-post-route (node)
  (when node
    (let ((route-name (intern-string (node-slug node)))
          (route-path (build-slug node)))
      (eval
       `(restas:define-route ,route-name (,route-path)
          (progn
            (sling-log 200)
            (show-node-post ,node)))))))

(defun build-page-route (node)
  (when node
    (let* ((route-path (build-slug node))
           (route-name (intern-string
                        (sanitize-static-slug route-path))))
      (eval
       `(restas:define-route ,route-name (,route-path)
          (progn
            (sling-log 200)
            (show-node-page ,node)))))))

(defun build-asset-route (path)
  (when path
    (let* ((route-path (strip-prefix (namestring path)
                                     (namestring *sinclair-root*)))
           (route-name (intern-string
                        (sanitize-static-slug route-path)))
           (content-type (if (ends-with path ".css")
                             "text/css"
                             "application/octet-stream")))

      (eval
       `(restas:define-route ,route-name (,route-path :method :get 
                                                      :content-type ,content-type)
          (progn
            (sling-log 200)
            (file-string ,path)))))))

(defun show-node-post (node)
  (concatenate 'string
               (<:doctype)
               (<:html
                (<:head
                 (<:title (node-title node))
                 (<:meta :charset "UTF-8")
                 (mapcar (lambda (style)
                           (<:link :type "text/css"
                                   :rel "stylesheet"
                                   :href style))
                         (load-stylesheets)))
                (<:body
                 (<:div :id "container"
                        (load-header)
                        (<:div :id "content"
                               (<:h2 (node-title node))
                               (<:h4 "Published: " (pretty-node-date node))
                               (<:h4 "Tags:"
                                     (format nil "~{ ~A~^,~}"
                                             (mapcar
                                              (lambda (tag)
                                                (<:a :href
                                                     (format nil "/tag/~A" tag)
                                                     tag))
                                              (node-tags node))))
                               (<:div :id "post-body"
                                      (node-body node))))))))

(defun show-node-page (node)
  (concatenate 'string
               (<:doctype)
               (<:html
                (<:head
                 (<:title (node-title node))
                 (<:meta :charset "UTF-8")
                 (mapcar (lambda (style)
                           (<:link :type "text/css"
                                   :rel "stylesheet"
                                   :href style))
                         (load-stylesheets)))
                (<:body
                 (<:div :id "container"
                        (load-header)
                        (<:div :id "content"
                               (<:h2 (node-title node))
                               (<:div :id "post-body"
                                      (node-body node))))))))

(defun reload-site (&key (from-disk nil))
  (when from-disk
    (let ((node-list (send-forth-minions *sinclair-root*)))
        (mapcar #'store-node
                (getf node-list :nodes))
        (mapcar #'build-asset-route
                (getf node-list :assets))))
  (mapcar #'build-route
          (load-all-nodes))
  (build-tags))


(defun build-tags ()
  (setf *site-tags* (make-hash-table :test #'equal))
  (mapcar (lambda (node) (store-node-tags node)) 
          (filter-pages (load-all-nodes)))
  (sort 
   (loop for key being the hash-keys of *site-tags* collecting key)
   #'string-lessp))

(defun store-node-tags (node)
  (dolist (tag (node-tags node))
    (multiple-value-bind (tagged-nodes present)
        (gethash tag *site-tags*)
      (declare (ignore present))
      (setf (gethash tag *site-tags*)
            (cons node tagged-nodes)))))

(restas:define-route tag-page ("/tag/:tag"
                                    :method :get)
  (concatenate 'string
               (<:doctype)
               (<:html
                (<:head
                 (<:title (or (gethash :title *site-config*) ""))
                 (<:meta :charset "UTF-8")
                 (mapcar (lambda (style)
                           (<:link :type "text/css"
                                   :rel "stylesheet"
                                   :href style))
                         (load-stylesheets)))
                (<:body
                 (<:div :id "container"
                        (load-header)
                        (<:div :id "content"
                               (<:h2 (format nil "Tag: ~A" tag))
                               (<:ul
                                (mapcar
                                 (lambda (node)
                                   (<:li
                                    (<:a :href
                                         (build-slug node)
                                         (node-title node))))
                                 (gethash tag *site-tags*)))))))))
