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
                  "GET-ATTACHMENT-ATTACHED-P"
                  "SET-SESSION"))
    (check (null (find-symbol name "MTM"))
           "The public package still exports ~A."
           name)))

(deftest named-api-ensures-and-enters-by-name ()
  (when (get-session-manager-value)
    (del-session-manager))
  (unwind-protect
       (progn
         (check (equal '(:state :stopped :sessions nil)
                      (get-session-manager))
                "The stopped Manager snapshot is wrong.")
         (check (null (get-session-list))
                "An absent Session manager does not return an empty list.")
         (let ((first-session (new-session-value "api-session"))
               (second-session nil))
           (setf second-session (new-session-value "api-session"))
           (check (eq first-session second-session)
                  "NEW SESSION creates a duplicate named Session.")
           (check (equal '(:state :running
                           :sessions (("api-session" . :running)))
                         (get-session-manager))
                  "The running Manager snapshot is wrong.")
           (check (eq first-session (get-session-value "api-session"))
                  "GET SESSION returns the wrong Session.")
           (check (string= "api-session" (session-name first-session))
                  "The Session keeps the wrong name.")
           (multiple-value-bind (width height)
               (get-session-size first-session)
             (check (and (= width 80) (= height 24))
                    "The Session reports the wrong fixed terminal size."))
           (check (equal '(("api-session" . :running))
                         (get-session-list))
                  "GET SESSION returns the wrong list.")
           (check (error-signals-p
                   (lambda () (get-session-value "missing")))
                  "GET SESSION accepts a missing name.")
           (check (eq first-session (del-session "api-session"))
                  "DEL SESSION returns the wrong Session.")
           (check (null (del-session "api-session"))
                  "DEL SESSION is not idempotent.")
           (check (null (get-session-list))
                  "DEL SESSION keeps the Session list entry.")))
    (when (get-session-manager-value)
      (del-session-manager))))

(deftest named-api-manages-manager-lifecycle-idempotently ()
  (when (get-session-manager-value)
    (del-session-manager))
  (let ((first-manager (new-session-manager))
        (second-manager nil))
    (unwind-protect
         (progn
           (setf second-manager (new-session-manager))
           (check (eq first-manager second-manager)
                  "NEW SESSION-MANAGER creates a duplicate Manager.")
           (new-session-value "first")
           (new-session-value "second")
           (check (equal '(:state :running
                           :sessions (("first" . :running)
                                      ("second" . :running)))
                         (get-session-manager))
                  "The Manager snapshot omits a Session.")
           (check (error-signals-p
                   (lambda () (get-session-value "")))
                  "GET SESSION accepts an empty name.")
           (check (error-signals-p
                   (lambda ()
                     (funcall (symbol-function 'mtm:get-session))))
                  "GET SESSION accepts no name.")
           (check (error-signals-p
                   (lambda ()
                     (funcall (symbol-function 'mtm:del-session))))
                  "DEL SESSION accepts no name.")
           (check (del-session-manager)
                  "DEL SESSION-MANAGER does not stop the Manager.")
           (check (equal '(:state :stopped :sessions nil)
                        (get-session-manager))
                  "DEL SESSION-MANAGER keeps the Manager state.")
           (check (null (del-session-manager))
                  "DEL SESSION-MANAGER is not idempotent."))
      (when (get-session-manager-value)
        (del-session-manager)))))

(deftest named-api-exposes-entering-session-functions ()
  (dolist (name '(mtm:get-session mtm:new-session))
    (check (fboundp name)
           "The public Session function is missing.")))
