(defpackage #:mtm.test.frontend
  (:use #:cl)
  (:import-from #:mtm.test
                #:capture-platform-output
                #:check
                #:deftest
                #:new-test-octets)
  (:import-from #:mtm.utf8
                #:get-utf8))

(in-package #:mtm.test.frontend)

(deftest frontend-forwards-input-bytes-unchanged ()
  (let ((frontend (mtm.frontend::new-session-frontend :socket-fd 9))
        (bytes (get-utf8
                (format nil "~C[<0;7;9Mls~C~C[A"
                        #\Escape #\Newline #\Escape))))
    (check (equalp
            (capture-platform-output
             (lambda ()
               (mtm.frontend::set-frontend-input-bytes frontend bytes)))
            bytes)
           "The frontend must forward input bytes unchanged.")))

(deftest frontend-parses-resize-control ()
  (multiple-value-bind (rows columns)
      (mtm.frontend::get-resize-control "resize;21;80")
    (check (and (= rows 21) (= columns 80))
           "The frontend parses the wrong terminal size.")))

(deftest frontend-sends-resize-control-on-socket ()
  (let ((frontend
          (mtm.frontend::new-session-frontend
           :socket-fd 9
           :rows 21
           :columns 80)))
    (check (equalp
            (capture-platform-output
             (lambda ()
               (mtm.frontend::set-session-size frontend)))
            (get-utf8
             (format nil "~C]MTM;resize;21;80~C" #\Escape (code-char 7))))
           "The socket frontend must send its terminal size.")
    (check (equalp
            (capture-platform-output
             (lambda ()
               (mtm.frontend::set-session-size frontend)))
            (new-test-octets))
           "The socket frontend must send one size only once.")))

(deftest frontend-strips-split-resize-control ()
  (let* ((frontend (mtm.frontend::new-session-frontend))
         (marker (get-utf8
                  (format nil "x~C]MTM;resize;21;80~Cy"
                          #\Escape
                          (code-char 7))))
         (ordinary (new-test-octets))
         (payloads nil))
    (labels ((consume (bytes)
               (mtm.frontend::set-mtm-controls
                frontend
                bytes
                (lambda (chunk)
                  (setf ordinary
                        (concatenate '(vector (unsigned-byte 8))
                                     ordinary chunk)))
                (lambda (payload)
                  (push payload payloads)))))
      (consume (subseq marker 0 4))
      (consume (subseq marker 4)))
    (check (equalp (get-utf8 "xy") ordinary)
           "The frontend forwards bytes around a control incorrectly.")
    (check (equal '("resize;21;80") payloads)
           "The frontend does not collect a split control.")))

(deftest frontend-forwards-incomplete-control-after-delay ()
  (let* ((frontend (mtm.frontend::new-session-frontend))
         (ordinary (new-test-octets)))
    (mtm.frontend::set-mtm-controls
     frontend
     (new-test-octets 27)
     (lambda (bytes)
       (setf ordinary
             (concatenate '(vector (unsigned-byte 8)) ordinary bytes)))
     (lambda (payload)
       (declare (ignore payload))))
    (setf (mtm.frontend::session-frontend-control-since frontend)
          (- (get-internal-real-time) internal-time-units-per-second))
    (mtm.frontend::set-pending-mtm-control-output
     frontend
     (lambda (bytes)
       (setf ordinary
             (concatenate '(vector (unsigned-byte 8)) ordinary bytes))))
    (check (equalp #(27) ordinary)
           "The frontend delays an incomplete control forever.")))
