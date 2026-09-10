(defpackage #:mtm.test
  (:use #:cl)
  (:import-from #:mtm.session
                #:get-attachment-output)
  (:import-from #:mtm.terminal
                #:get-terminal-screen-lines)
  (:export
   #:attachment-output-has-text-p
   #:bytes-to-string
   #:capture-platform-output
   #:check
   #:deftest
   #:error-signals-p
   #:new-test-octets
   #:screen-has-text-p
   #:set-tests
   #:wait-until))

(in-package #:mtm.test)

(defvar *tests* nil)

(defmacro deftest (name () &body body)
  `(progn
     (defun ,name () ,@body)
     (pushnew ',name *tests*)))

(defun check (condition format-control &rest format-arguments)
  (unless condition
    (error (apply #'format nil format-control format-arguments))))

(defun error-signals-p (function)
  "Return true when FUNCTION signals an error."
  (handler-case
      (progn
        (funcall function)
        nil)
    (error () t)))

(defun bytes-to-string (bytes)
  (map 'string #'code-char bytes))

(defun new-test-octets (&rest values)
  (make-array (length values)
              :element-type '(unsigned-byte 8)
              :initial-contents values))

(defun screen-has-text-p (terminal text)
  (some (lambda (line) (search text line))
        (get-terminal-screen-lines terminal)))

(defun wait-until (predicate &key (attempts 100) (delay 0.05))
  (loop repeat attempts
        when (funcall predicate)
          do (return t)
        do (sleep delay)
        finally (return (funcall predicate))))

(defun attachment-output-has-text-p (attachment text)
  (let ((output ""))
    (loop repeat 200
          do (multiple-value-bind (bytes eof-p)
                 (get-attachment-output attachment :wait-p nil)
               (when (and bytes (plusp (length bytes)))
                 (setf output
                       (concatenate 'string output (bytes-to-string bytes)))
                 (when (search text output)
                   (return-from attachment-output-has-text-p t)))
               (when eof-p
                 (return nil))
               (sleep 0.01)))
    nil))

(defun capture-platform-output (function)
  "Capture bytes written through the platform output boundary."
  (let ((bytes (new-test-octets))
        (original (symbol-function 'mtm.platform:set-fd)))
    (unwind-protect
         (progn
           (setf (symbol-function 'mtm.platform:set-fd)
                 (lambda (fd output)
                   (declare (ignore fd))
                   (setf bytes
                         (concatenate '(vector (unsigned-byte 8))
                                      bytes
                                      output))
                   (length output)))
           (funcall function)
           bytes)
      (setf (symbol-function 'mtm.platform:set-fd) original))))

(defun set-tests ()
  (let ((passed 0)
        (failed 0))
    (dolist (test (reverse *tests*))
      (handler-case
          (progn
            (funcall test)
            (incf passed)
            (format t "PASS ~A~%" test))
        (error (condition)
          (incf failed)
          (format t "FAIL ~A: ~A~%" test condition))))
    (format t "~A passed, ~A failed.~%" passed failed)
    (when (plusp failed)
      (error "The test suite has failures."))
    t))
