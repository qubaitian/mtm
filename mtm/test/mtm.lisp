(defpackage #:mtm.test.mtm
  (:use #:cl)
  (:import-from #:mtm.test
                #:check
                #:deftest))

(in-package #:mtm.test.mtm)

(deftest public-surface-keeps-only-session-functions ()
  (check (null (find-package "MTM.INPUT"))
         "The removed input package still exists.")
  (check (null (find-package "MTM.LOGGING"))
         "The removed logging package still exists.")
  (check (null (find-package "MTM.API"))
         "The removed generic API package still exists.")
  (dolist (name '("MTM.EDITOR" "MTM.COMPLETION" "MTM.PROMPT"))
    (check (null (find-package name))
           "The removed ~A package still exists."
           name))
  (dolist (name '("SET-COMMAND-FRONTEND"
                  "SET-INTERACTIVE-SHELL"
                  "GET-TERMINAL-RENDER"
                  "SET-MANAGER-DEBUG-VALUE"
                  "GET-SESSION-MANAGER-STATE"
                  "GET-SESSION-BY-NAME"
                  "GET-SESSION-RUNNING-P"
                  "GET-ATTACHMENT-ATTACHED-P"
                  "SESSION-ID"))
    (check (null (find-symbol name "MTM"))
           "The public package still exports ~A."
           name))
  (dolist (name '("NEW-EDITOR"
                  "SET-EDITOR-BYTE"
                  "GET-EDITOR-HISTORY"))
    (check (null (find-symbol name "MTM"))
           "The public package still exports ~A."
           name))
  (dolist (name '("SET-PROMPT-PROVIDER" "SET-PROMPT-TEMPLATE"))
    (check (null (find-symbol name "MTM"))
           "The public package still exports ~A."
           name))
  (multiple-value-bind (symbol status)
      (find-symbol "SET-PASSTHROUGH-FRONTEND" "MTM")
    (check (and symbol (eq status :external))
           "The public package does not export passthrough.")))

