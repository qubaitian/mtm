(in-package #:mtm.tests)

;; Return one unique temporary path for a Service source test.
(defun new-service-test-path (label)
  "Return one unique temporary path for a Service source test."
  (merge-pathnames
   (format nil "mtm-service-~A-~D-~D.lisp"
           label
           (get-universal-time)
           (random 1000000))
   (uiop:temporary-directory)))

;; Return one source form for a named Service.
(defun new-service-test-form (name program)
  "Return one source form for a named Service."
  (list 'mtm:new-service
        :name name
        :program (list 'quote program)))

;; Replace one temporary Service source with FORMS.
(defun set-service-test-source (path forms)
  "Replace one temporary Service source with FORMS."
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (form forms)
      (write form :stream stream :readably t)
      (terpri stream)))
  path)

;; Remove one temporary Service source after a test.
(defun del-service-test-source (path)
  "Remove one temporary Service source after a test."
  (ignore-errors (delete-file path)))

;; Return true when SERVICE output contains TEXT.
(defun service-output-has-text-p (name text)
  "Return true when SERVICE output contains TEXT."
  (search text (bytes-to-string (get-service-output name))))

(deftest services-use-unique-global-names ()
  (when (get-session-manager-value)
    (del-session-manager))
  (unwind-protect
       (progn
         (new-session-manager)
         (new-session-value "shared-session")
         (new-service
          :name "shared-service"
          :program (list "/bin/sh" "-c" "sleep 10"))
         (check (error-signals-p
                 (lambda ()
                   (new-session-value "shared-service")))
                "A Session duplicates a Service name.")
         (check (error-signals-p
                 (lambda ()
                   (new-service
                    :name "shared-session"
                    :program (list "/bin/sh" "-c" "sleep 10"))))
                "A Service duplicates a Session name.")
         (check (error-signals-p
                 (lambda ()
                   (new-service
                    :name "shared-service"
                    :program (list "/bin/sh" "-c" "sleep 10"))))
                "A Service name is not unique."))
    (when (get-session-manager-value)
      (del-session-manager))))

(deftest services-run-stop-and-retain-output ()
  (when (get-session-manager-value)
    (del-session-manager))
  (unwind-protect
       (progn
         (new-service
          :name "output-service"
          :program (list "/bin/sh" "-c" "printf service-marker; sleep 10"))
         (check (wait-until
                 (lambda ()
                   (service-output-has-text-p
                    "output-service"
                    "service-marker")))
                "The Service does not retain PTY output.")
         (let ((snapshot (set-service "output-service" :stopped)))
           (check (and (eq :stopped (getf snapshot :desired-state))
                       (wait-until
                        (lambda ()
                          (and (eq :stopped
                                   (getf (get-service "output-service") :state))
                               (null (getf (get-service "output-service") :pid))))))
                  "SET SERVICE does not stop the child process."))
         (check (getf (del-service "output-service") :name)
                "DEL SERVICE does not return its snapshot.")
         (check (error-signals-p
                 (lambda () (get-service "output-service")))
                "DEL SERVICE keeps the Service registered."))
    (when (get-session-manager-value)
      (del-session-manager))))

(deftest services-restart-after-natural-exit ()
  (when (get-session-manager-value)
    (del-session-manager))
  (unwind-protect
       (progn
         (new-service
          :name "restart-service"
          :program (list "/bin/sh" "-c" "printf R; exit 0"))
         (check (wait-until
                 (lambda ()
                   (>= (count #\R
                              (bytes-to-string
                               (get-service-output "restart-service")))
                       2))
                 :attempts 260
                 :delay 0.01)
                "The Service does not restart after natural exit."))
    (when (get-session-manager-value)
      (del-session-manager))))

(deftest service-sources-reconcile-atomically ()
  (when (get-session-manager-value)
    (del-session-manager))
  (let ((first-path (new-service-test-path "first"))
        (second-path (new-service-test-path "second")))
    (unwind-protect
         (progn
           (set-service-test-source
            first-path
            (list (new-service-test-form
                   "source-one"
                   (list "/bin/sh" "-c" "sleep 10"))))
           (set-service-test-source
            second-path
            (list (new-service-test-form
                   "source-two"
                   (list "/bin/sh" "-c" "sleep 10"))))
           (new-service-source first-path)
           (new-service-source second-path)
           (check (and (get-service "source-one")
                       (get-service "source-two"))
                  "Multiple Service sources do not load.")
           (sleep 1.1)
           (set-service-test-source
            first-path
            (list (new-service-test-form
                   "source-three"
                   (list "/bin/sh" "-c" "sleep 10"))))
           (new-service-source first-path)
           (check (and (error-signals-p
                        (lambda () (get-service "source-one")))
                       (get-service "source-three"))
                  "A source does not remove deleted declarations.")
           (del-service "source-three")
           (new-service-source first-path)
           (check (get-service "source-three")
                  "An unchanged source does not restore its declaration.")
           (sleep 1.1)
           (set-service-test-source
            second-path
            (list (new-service-test-form
                   "source-three"
                   (list "/bin/sh" "-c" "sleep 10"))))
           (check (error-signals-p
                   (lambda () (new-service-source second-path)))
                  "Two Service sources share one name.")
           (check (get-service "source-two")
                  "A rejected source changed existing Services.")
           (sleep 1.1)
           (set-service-test-source
            second-path
            (list (new-service-test-form
                   "source-two"
                   (list "/bin/sh" "-c" "sleep 10"))
                  (new-service-test-form
                   "source-two"
                   (list "/bin/sh" "-c" "sleep 10"))))
           (check (error-signals-p
                   (lambda () (new-service-source second-path)))
                  "One source accepts duplicate declarations.")
           (check (get-service "source-two")
                  "An invalid source changed existing Services."))
      (del-service-test-source first-path)
      (del-service-test-source second-path)
      (when (get-session-manager-value)
        (del-session-manager)))))
