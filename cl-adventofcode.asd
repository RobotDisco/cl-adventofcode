;;;; cl-adventofcode.asd

(defsystem cl-adventofcode
    :author "Gaelan D'costa <gdcosta@gmail.com>"
    :maintainer "Gaelan D'costa <gdcosta@gmail.com>"
    :license "GPLv3"
    :homepage "https://github.com/RobotDisco/cl-adventofcode"
    :version "0.1"
    :depends-on ()
    :components ((:module "src"
			  :serial t
			  :components
			  ((:file "cl-adventofcode"))))
    :description "My attempt at Advent of Code 2020"
    :long-description
    #.(uiop:read-file-string
       (uiop:subpathname *load-pathname* "README.md"))
    :in-order-to ((test-op (test-op cl-adventofcode-test))))
