;;;; envmgr.lisp

(in-package #:envmgr)

(defun handle-alias (original new)
  (format t "'~a' is now '~a'~%" original new))

(defun handle-print (content)
  (iter (for pair in content)
    (if pair
	(format t "~a~%" pair))))

;; TODO: import clj-arrows instead
(defun handle-file-line (line)
  (let ((str (coerce line 'string)) (type nil))
    (as-> (uiop:split-string str) $
	  (progn
	    (setf type (first $))
	    (cdr $))
	  (apply #'uiop:strcat $)
	  (uiop:split-string $ :separator "=")
	  (mapcar #'(lambda (s) (remove #\' s)) $)
	  (append $ (list (read-from-string type))))))

(defun main (argv)
  "Usage: envmgr [options]

Options:
  -a, --alias <command> <replacement>
  -v, --variable <variable> <value>
  -p, --print
  -i, --interactive"
  
  (with-open-file (file "aliases")
    (let ((content (mapcar #'handle-file-line (uiop:read-file-lines file))))
      (parse-args argv
		  ("--help" (lambda () (format t "~a" (documentation #'main 'function))))
		  ("--alias" #'handle-alias)
		  ("--print" (lambda () (handle-print content)))))))
