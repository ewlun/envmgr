;;;; envmgr.lisp

(in-package #:envmgr)


(defun main (&optional argv)
  (parse-args argv
	      ("--alias" (lambda ()))))
