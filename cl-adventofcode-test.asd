(defsystem cl-adventofcode-test
    :author "Gaelan D'costa <gdcosta@gmail.com>"
    :license "GPLv3"
    :depends-on (:cl-adventofcode)
    :components ((:module "t"
			  :serial t
			  :components
			  ((:file "cl-adventofcode")))))
