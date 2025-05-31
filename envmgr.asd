;;;; envmgr.asd

(asdf:defsystem #:envmgr
  :description "Program that can manage environment variables and aliases"
  :author "Edwin Lundmark"
  :license  "GPL"
  :version "0.0.1"
  :serial t
  :depends-on (:iterate)
  :components ((:file "package")
	       (:file "argparse")
               (:file "envmgr")))
