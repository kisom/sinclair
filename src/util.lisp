(in-package :sinclair)

;;; utilities and other such nonsense and minutæ

(defun compose (&rest fns)
  "Compose allows a number of functions with the same arity to be composed together in a chain."
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(defun to-keyword (sym)
  "Convert the symbol to a keyword."
  (intern (string sym) "KEYWORD"))

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

(defun unix-to-timestamp (timestamp)
  (local-time:unix-to-timestamp
   (if (numberp timestamp)
       timestamp
       (parse-integer timestamp))))

(defun tags-as-keywords (tags)
  "Convert a list of tag symbols to keywords"
  (when (listp tags)
    (mapcar #'to-keyword (mapcar #'string tags))))

(defun tags-as-strings (tags)
  "Convert a list of tag keywords to strings"
  (mapcar #'string-downcase
          (mapcar #'string
                  tags)))
