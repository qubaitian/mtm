(asdf:defsystem "mtm"
  :description "MTM provides a small Common Lisp terminal multiplexer."
  :version "0.1.0"
  :depends-on ("bordeaux-threads" "cffi" "babel")
  :serial t
  :components ((:file "src/package")
               (:file "src/utf8")
               (:file "src/platform")
               (:file "src/editor")
               (:file "src/pty")
               (:file "src/service")
               (:file "src/terminal")
               (:file "src/session")
               (:file "src/frontend"))
  :in-order-to ((test-op (test-op "mtm/tests"))))

(asdf:defsystem "mtm/app"
  :description "The MTM command-line application."
  :version "0.1.0"
  :depends-on ("mtm"
               "clingon"
               "usocket"
               "babel"
               "clack"
               "clack-handler-hunchentoot"
               "websocket-driver-server"
               "yason")
  :serial t
  :components ((:file "app/package")
               (:file "app/browser")
               (:file "app/manager")
               (:file "app/main"))
  :build-operation "program-op"
  :build-pathname "bin/mtm"
  :entry-point "mtm.app:main")

(asdf:defsystem "mtm/tests"
  :depends-on ("mtm" "mtm/app")
  :serial t
  :components ((:file "tests/package")
               (:file "tests/test")
               (:file "tests/app-test")
               (:file "tests/session-api-test")
               (:file "tests/service-test"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call :mtm.tests :set-tests)))
