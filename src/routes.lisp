(in-package :sinclair)

;;; page generation macros and routes

(defmacro load-and-index-nodes-by-year ()
  `(mapcar #'index-for-year
           (group-nodes-by-year
            (filter-pages
             (load-all-nodes)))))

(defmacro invalidate-route (route-name route-url)
  `(restas:define-route ,route-name (,route-url)
     (restas:redirect 'index-route)))

(restas:define-route index-route ("/" :method :get)
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
                               (load-and-index-nodes-by-year)))))))

(restas:define-route blog-rss ("/index.rss" :method :get)
  (rss-feed))

(defun build-route (node)
  (when node
    (case (node-mode node)
      (:page (build-page-route node))
      (:post (build-post-route node))
      (otherwise nil))))

(defun build-post-route (node)
  (when node
    (let ((route-name (intern-string (node-slug node)))
          (route-path (build-slug node)))
      (eval
       `(restas:define-route ,route-name (,route-path)
          (show-node-post ,node))))))

(defun build-page-route (node)
  (when node
    (let* ((route-path (build-slug node))
           (route-name (intern-string
                        (sanitize-static-slug route-path))))
      (eval
       `(restas:define-route ,route-name (,route-path)
          (show-node-page ,node))))))

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
          (file-string ,path))))))

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
                               (<:h4 "Tags:" (format nil "~{ ~A~^,~}" (node-tags node)))
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
          (load-all-nodes)))
