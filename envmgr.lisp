;;;; envmgr.lisp

(in-package #:envmgr)

;; Takes in a new pair, checks if it exists in the file.
;; If it does, it replaces the old value with the new one.
;; If not, it adds a new entry.

(defun change-value (original new type content)
  (let ((found-value (find-if #'(lambda (pair) (and
				     (string= (first pair) original)
				     (eq (third pair) type)))
			      content)))
    (if found-value
	(setf (second found-value) new)
	(setf content (nconc content (list (list original new type)))))))

(defun handle-print (content)
  (iter (for pair in content)
    (if pair
	(format t "~s~%" pair))))

;; TODO: import clj-arrows instead
(defun handle-file-line (line)
  (if (> (length line) 0)
      (let ((str (coerce line 'string)) (type nil))
	(as-> (str:words str) $
	      (progn
		(setf type (first $))
		(cdr $))
	      (str:unwords $)
	      (str:split "=" $)
	      (mapcar #'(lambda (s) (remove #\' s)) $)
	      (mapcar #'(lambda (s) (remove #\" s)) $)
	      (append $ (list (read-from-string type)))))))

(defun rebuild-file (content stream)
  (iter (for pair in content)
    (if (third pair)
	(format stream "~a ~s=~s~%"
	     (sb-unicode:lowercase (symbol-name (third pair)))
	     (first pair)
	     (second pair))
	(format stream "~%"))))

(defun main (argv)
  "Usage: envmgr [options]

Options:
  -a, --alias <command> <replacement>
  -v, --variable <variable> <value>
  -p, --print
  -i, --interactive"
  
  (with-open-file (file "aliases" :direction :io :if-exists :overwrite)
    (let* ((lines (uiop:read-file-lines file)) (content (mapcar #'handle-file-line lines)))
      (parse-args argv
		  ("--help" (lambda () (format t "~a" (documentation #'main 'function))))
		  ("--alias" (lambda (original new) (change-value original new 'alias content)))
		  ("--print" (lambda () (handle-print content))))
      (rebuild-file content file))))
