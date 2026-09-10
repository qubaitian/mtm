(asdf:defsystem "mtm"
  :description "MTM provides a small Common Lisp terminal multiplexer."
  :version "0.1.0"
  :depends-on ("bordeaux-threads" "babel"
               "clingon"
               "usocket"
               "sb-posix"
               "sb-bsd-sockets")
  :serial t
  :components ((:file "mtm/utf8")
               (:file "mtm/platform")
               (:file "mtm/pty")
               (:file "mtm/service")
               (:file "mtm/terminal")
               (:file "mtm/session")
               (:file "mtm/frontend")
               (:file "mtm/client")
               (:file "mtm")
               (:file "mtm/manager")
               (:file "mtm/cli"))
  :in-order-to ((test-op (test-op "mtm/tests"))))

(asdf:defsystem "mtm/tests"
  :description "Test suite for MTM."
  :version "0.1.0"
  :depends-on ("mtm")
  :serial t
  :pathname "mtm"
  :components ((:file "test")
               (:file "test/utf8")
               (:file "test/platform")
               (:file "test/pty")
               (:file "test/terminal")
               (:file "test/frontend")
               (:file "test/mtm")
               (:file "test/integration")
               (:file "test/cli")
               (:file "test/session")
               (:file "test/service"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call :mtm.test :set-tests)))
