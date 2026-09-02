(in-package #:mtm.tests)

(defun error-signals-p (function)
  "Return true when FUNCTION signals an error."
  (handler-case
      (progn
        (funcall function)
        nil)
    (error () t)))

(deftest named-api-removes-generic-package ()
  (check (null (find-package "MTM.API"))
         "The removed generic API package still exists.")
  (dolist (name '("GET-SESSION-MANAGER-STATE"
                  "GET-SESSION-BY-NAME"
                  "GET-SESSION-RUNNING-P"
                  "GET-ATTACHMENT-ATTACHED-P"))
    (check (null (find-symbol name "MTM"))
           "The public package still exports ~A."
           name)))

(deftest named-api-uses-a-lazy-global-manager ()
  (when (get-session-manager)
    (del-session-manager))
  (unwind-protect
       (progn
         (check (null (get-session-manager))
                "The Session manager exists before creation.")
         (check (null (get-session-list))
                "An absent Session manager does not return an empty list.")
         (let ((session (new-session "api-session")))
           (check (get-session-manager)
                  "NEW SESSION does not create the global manager.")
           (check (eq session (get-session "api-session"))
                  "GET SESSION returns the wrong Session.")
           (check (string= "api-session" (session-name session))
                  "The Session keeps the wrong name.")
           (multiple-value-bind (width height)
               (get-session-size session)
             (check (and (= width 80) (= height 24))
                    "The Session reports the wrong fixed terminal size."))
           (check (equal '(("api-session" . :running))
                         (get-session-list))
                  "GET SESSION returns the wrong list.")
           (check (error-signals-p
                   (lambda () (new-session "api-session")))
                  "NEW SESSION accepts a duplicate name.")
           (check (error-signals-p
                   (lambda () (get-session "missing")))
                  "GET SESSION accepts a missing name.")
           (check (eq session (del-session "api-session"))
                  "DEL SESSION returns the wrong Session.")
           (check (null (get-session-list))
                  "DEL SESSION keeps the Session list entry.")))
    (when (get-session-manager)
      (del-session-manager))))

(deftest named-api-manages-manager-lifecycle ()
  (when (get-session-manager)
    (del-session-manager))
  (let ((manager (new-session-manager)))
    (unwind-protect
         (progn
           (check (eq manager (get-session-manager))
                  "NEW SESSION-MANAGER does not store the Manager.")
           (check (error-signals-p
                   (lambda () (new-session-manager)))
                  "NEW SESSION-MANAGER accepts a duplicate.")
           (check (error-signals-p
                   (lambda () (get-current-session)))
                  "GET CURRENT-SESSION accepts a missing position.")
           (check (eq manager (del-session-manager))
                  "DEL SESSION-MANAGER returns the wrong Manager.")
           (check (null (get-session-manager))
                  "DEL SESSION-MANAGER keeps the Manager."))
      (when (get-session-manager)
        (del-session-manager)))))
