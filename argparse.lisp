(in-package #:envmgr)

(defmacro parse-args (args &rest pairs)
  "args - list of strings '('envmgr' '--alias' ''ls -lah'' 'ls')
   pairs - pairs of string and function ('string' (lambda (arg1 ...) ...))"
  
  (iter (for pair in pairs)
    (let ((arg (first pair)) (fn (second pair)))
      (collect `(,arg ,fn
		      ,(if (eq 'FUNCTION (car (second pair)))
			   (length (sb-introspect:function-lambda-list (cadr (second pair))))
			   (length (second fn))))
	into new-pairs))
    (finally
     (return `(parse-args-fun ,args
			      ,@(mapcar (lambda (pair) `(list ,@pair)) new-pairs))))))

;; Process:
;; 1. Does first word match the word of any pair?
;;    a) If so, advance the same amount of words that the
;;       function has arguments
;;
;;    b) If not, throw an error
;;
;; 2. Repeat until there are no words left

(defun parse-args-fun (args &rest pairs)
  (iter (for i from 1 to (- (length args) 1))
    (let ((match (iter (for pair in pairs)
		   (if (string= (nth i args) (first pair)) (leave pair))
		   (finally (error "Unknown argument: ~a~%" (nth i args))))))

      (let* ((fn (second match)) (len (third match)))
	(if (> (+ i len 1) (length args))
	    (error "Not enough arguments given to ~a~%" (first match)))
	(apply fn (subseq args (+ i 1) (+ i len 1)))
	(SETF i (+ i len))))))

(parse-args '("main.exe" "--alias" "ls -lah" "ls" "--print")
	    ("--alias" (lambda (one two) (format t "Aliasing ~a to ~a~%" one two)))
	    ("--print" (lambda () (format t "Printing out everything~%")))
	    ("--test" #'+))
