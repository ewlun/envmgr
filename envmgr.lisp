;;;; envmgr.lisp

(in-package #:envmgr)

(defun handle-alias (original new)
  (format t "'~a' is now '~a'~%" original new))

(defun main (argv)
  (with-open-file (file "aliases")
    (let ((content (uiop:read-file-lines file)))
      (parse-args argv
	      ("--alias" #'handle-alias)
      	      ("--print" (lambda ()
			   (format t "~a~%" (first content))))))))
