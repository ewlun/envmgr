;;;; envmgr.lisp

(in-package #:envmgr)

(defun change-value (original new type content lines)
  (let ((found-value
	  (find-if #'(lambda (pair) (and
				     (string= (first pair) original)
				     (eq (third pair) type)))
		   content))
	(found-line
	  (find-if #'(lambda (line)
		       (numberp (or
				 (search (uiop:strcat original "=") line)
				 (search (uiop:strcat original "'=") line))))
		   lines)))
    (format t "~a ~a ~a ~%" found-line found-value new)))

(defun handle-print (content)
  (iter (for pair in content)
    (if pair
	(format t "~a~%" pair))))

;; TODO: import clj-arrows instead
(defun handle-file-line (line)
  (if (> (length line) 0)
      (let ((str (coerce line 'string)) (type nil))
	(as-> (uiop:split-string str) $
	      (progn
		(setf type (first $))
		(cdr $))
	      (apply #'uiop:strcat $)
	      (uiop:split-string $ :separator "=")
	      (mapcar #'(lambda (s) (remove #\' s)) $)
	      (append $ (list (read-from-string type)))))))

(defun main (argv)
  "Usage: envmgr [options]

Options:
  -a, --alias <command> <replacement>
  -v, --variable <variable> <value>
  -p, --print
  -i, --interactive"
  
  (with-open-file (file "aliases")
    (let* ((lines (uiop:read-file-lines file)) (content (mapcar #'handle-file-line lines)))
      (parse-args argv
		  ("--help" (lambda () (format t "~a" (documentation #'main 'function))))
		  ("--alias" (lambda (original new) (change-value original new 'alias content lines))
		  ("--print" (lambda () (handle-print content))))))))
