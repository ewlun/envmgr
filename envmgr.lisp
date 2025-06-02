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
  (let* ((lengths (iter (for pair in content)
		    (maximize (length (first pair)) into k)
		    (maximize (length (second pair)) into v)
		    (maximize (length (string  (third pair))) into tl)
		    (finally (return (list k v tl)))))
	 (key (+ 2 (first lengths)))
	 (value (+ 2 (second lengths)))
	 (type (+ 2 (third lengths))))

    (let ((header (format nil "| ~v,,,' :@<Key~> | ~v,,,' :@<Value~> | ~v,,,' :@<Type~> |"
			  (- key 2) (- value 2) (- type 2))))

      (format t "+~v,,,'-<-~>+~v,,,'-<-~>+~v,,,'-<-~>+~%" key value type)
      (format t "~a~%" header)
      (format t "+~v,,,'-<-~>+~v,,,'-<-~>+~v,,,'-<-~>+~%" key value type))
    
    (iter (for pair in content)
      (if pair
	  (format t "|~v,,,' :@<~a~>|~v,,,' :@<~a~>|~v,,,' :@<~a~>|~%"
		  key (first pair)
		  value (second pair)
		  type (third pair))))
    
    (format t "+~v,,,'-<-~>+~v,,,'-<-~>+~v,,,'-<-~>+~%" key value type))))


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

(defun main (&optional argv)
  "Usage: envmgr [options]

Options:
  -a, --alias <command> <replacement>
  -v, --variable <variable> <value>
  -p, --print
  -i, --interactive"

  (or argv (setf argv sb-ext:*posix-argv*))
  
  (with-open-file (file "aliases" :direction :io :if-exists :overwrite)
    (let* ((lines (uiop:read-file-lines file)) (content (mapcar #'handle-file-line lines)))
      (parse-args argv
		  ("--help" (lambda () (format t "~a~%" (documentation #'main 'function))))
		  ("--alias" (lambda (original new) (change-value original new 'alias content)))
		  ("--variable" (lambda (variable value) (change-value variable value 'export content)))
		  ("--print" (lambda () (handle-print content))))
      (rebuild-file content file))))
