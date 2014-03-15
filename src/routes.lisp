(in-package :sinclair)

;;; page generation macros and routes

(defmacro load-and-index-nodes-by-year ()
  `(mapcar #'index-for-year
          (group-nodes-by-year
           (filter-pages
            (load-all-nodes-from-redis)))))

(defmacro generate-index ()
  `(string-trim *trailing-whitespace*
                (cl-who:with-html-output-to-string (s nil :prologue :html :indent t)
                  (:html
                   (:head
                    (:title ,(redis:red-hget "sinclair" :title))
                    (:meta :charset "UTF-8")
                    ,@(mapcar (lambda (style)
                                `(:link :type "text/css"
                                        :rel "stylesheet"
                                        :href ,style))
                              (read-from-string (load-stylesheets))))
                   (:body
                    (:div :id "container"
                          ,(load-header)
                          (:div :id "content"
                                (:h2 "Index")
                                ,@(load-and-index-nodes-by-year))))))))

(defun generate-node-post-route (node)
  (let ((node-url (build-slug node)))
    (eval
     `(define-route ,(intern (string-upcase (node-slug node)))
          (,node-url :method :get)
        `(string-trim *trailing-whitespace*
                      (cl-who:with-html-output-to-string (s nil :prologue :html :indent t)
                        (:html
                         (:head
                          (:title ,title)
                          (:meta :charset "UTF-8")
                          ,@(mapcar (lambda (style)
                                      `(:link :type "text/css"
                                              :rel "stylesheet"
                                              :href ,style))
                                    (read-from-string (load-stylesheets))))
                         (:body
                          (:div :id "container"
                                (:div :id "header"
                                      ,(load-header))
                                (:div :id "content"
                                      (:h2 ,title)
                                      (:h4 Published ,published)
                                      (:div :id "post-body")
                                      ,body))))))))))
;;; routes

(restas:define-route index-route ("/" :method :get)
      (generate-index))

(defmacro invalidate-route (route-name route-url)
  `(restas:define-route ,route-name (,route-url)
     (restas:redirect 'index-route)))

