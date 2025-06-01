;;;; envmgr.lisp

(in-package #:envmgr)

(defun handle-alias (original new)
  (format t "'~a' is now '~a'~%" original new))


(defun main (argv)
  "Usage: envmgr [options]

Options:
  -a, --alias <command> <replacement>
  -v, --variable <variable> <value>
  -p, --print
  -i, --interactive"
  
  (with-open-file (file "aliases")
    (let ((content (uiop:read-file-lines file)))
      (parse-args argv
		  ("--help" (lambda () (format t "~a" (documentation #'main 'function))))
		  ("--alias" #'handle-alias)
		  ("--print" (lambda ()
			       (format t "~a~%" (first content))))))))
