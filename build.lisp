(asdf:load-asd "envmgr.asd")
(ql:quickload "envmgr")

(defun print-error-and-abort (e h)
  (declare (ignore h))
  (format *error-output* "~&~a~&" e)
  (sb-ext:exit :code 1)) 

(setf sb-ext:*invoke-debugger-hook* #'print-error-and-abort)

(sb-ext:save-lisp-and-die "envmgr"
			  :toplevel #'envmgr:main
			  :executable t
			  :compression t)

