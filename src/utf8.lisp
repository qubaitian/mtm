(in-package #:mtm.utf8)

(defun set-utf8-byte (bytes byte)
  (vector-push-extend byte bytes))

(defun get-encoded-code-point (bytes code-point)
  (cond
    ((<= code-point #x7f)
     (set-utf8-byte bytes code-point))
    ((<= code-point #x7ff)
     (set-utf8-byte bytes (+ #xc0 (ldb (byte 5 6) code-point)))
     (set-utf8-byte bytes (+ #x80 (ldb (byte 6 0) code-point))))
    ((<= code-point #xffff)
     (set-utf8-byte bytes (+ #xe0 (ldb (byte 4 12) code-point)))
     (set-utf8-byte bytes (+ #x80 (ldb (byte 6 6) code-point)))
     (set-utf8-byte bytes (+ #x80 (ldb (byte 6 0) code-point))))
    (t
     (set-utf8-byte bytes (+ #xf0 (ldb (byte 3 18) code-point)))
     (set-utf8-byte bytes (+ #x80 (ldb (byte 6 12) code-point)))
     (set-utf8-byte bytes (+ #x80 (ldb (byte 6 6) code-point)))
     (set-utf8-byte bytes (+ #x80 (ldb (byte 6 0) code-point))))))

(defun get-utf8 (text)
  "Encode TEXT as a UTF-8 octet vector."
  (check-type text string)
  (let ((bytes (make-array 16
                           :element-type '(unsigned-byte 8)
                           :adjustable t
                           :fill-pointer 0)))
    (loop for character across text
          for code-point = (char-code character)
          do (if (or (> code-point #x10ffff)
                     (<= #xd800 code-point #xdfff))
                 (get-encoded-code-point bytes #xfffd)
                 (get-encoded-code-point bytes code-point)))
    bytes))

(defun continuation-byte-p (byte)
  (<= #x80 byte #xbf))

(defun valid-utf8-sequence-p (bytes start length)
  (let ((first (aref bytes start)))
    (case length
      (2 (and (<= #xc2 first #xdf)
              (continuation-byte-p (aref bytes (1+ start)))))
      (3 (and (continuation-byte-p (aref bytes (+ start 2)))
              (or (and (= first #xe0)
                       (<= #xa0 (aref bytes (1+ start)) #xbf))
                  (and (= first #xed)
                       (<= #x80 (aref bytes (1+ start)) #x9f))
                  (and (<= #xe1 first #xec)
                       (<= #x80 (aref bytes (1+ start)) #xbf))
                  (and (<= #xee first #xef)
                       (<= #x80 (aref bytes (1+ start)) #xbf)))))
      (4 (and (continuation-byte-p (aref bytes (+ start 2)))
              (continuation-byte-p (aref bytes (+ start 3)))
              (or (and (= first #xf0)
                       (<= #x90 (aref bytes (1+ start)) #xbf))
                  (and (= first #xf4)
                       (<= #x80 (aref bytes (1+ start)) #x8f))
                  (and (<= #xf1 first #xf3)
                       (<= #x80 (aref bytes (1+ start)) #xbf)))))
      (otherwise nil))))

(defun get-utf8-sequence-length (byte)
  (cond
    ((<= byte #x7f) 1)
    ((<= #xc2 byte #xdf) 2)
    ((<= #xe0 byte #xef) 3)
    ((<= #xf0 byte #xf4) 4)
    (t nil)))

(defun get-decoded-code-point (bytes start length)
  (case length
    (2 (+ (ash (logand (aref bytes start) #x1f) 6)
          (logand (aref bytes (1+ start)) #x3f)))
    (3 (+ (ash (logand (aref bytes start) #x0f) 12)
          (ash (logand (aref bytes (1+ start)) #x3f) 6)
          (logand (aref bytes (+ start 2)) #x3f)))
    (4 (+ (ash (logand (aref bytes start) #x07) 18)
          (ash (logand (aref bytes (1+ start)) #x3f) 12)
          (ash (logand (aref bytes (+ start 2)) #x3f) 6)
          (logand (aref bytes (+ start 3)) #x3f)))))

(defun get-utf8-chunk (bytes &optional pending)
  "Decode BYTES and return text plus incomplete trailing bytes."
  (check-type bytes vector)
  (when pending
    (check-type pending vector))
  (let* ((input (if pending
                   (concatenate '(vector (unsigned-byte 8)) pending bytes)
                   bytes))
         (length (length input))
         (index 0)
         (text (make-array 16 :element-type 'character
                              :adjustable t
                              :fill-pointer 0))
         (remaining nil))
    (loop while (< index length)
          for byte = (aref input index)
          for sequence-length = (get-utf8-sequence-length byte)
          do (cond
               ((= sequence-length 1)
                (vector-push-extend (code-char byte) text)
                (incf index))
               ((null sequence-length)
                (vector-push-extend (code-char #xfffd) text)
                (incf index))
               ((< (- length index) sequence-length)
                (setf remaining (subseq input index))
                (setf index length))
               ((valid-utf8-sequence-p input index sequence-length)
                (vector-push-extend
                 (code-char (get-decoded-code-point input index sequence-length))
                 text)
                (incf index sequence-length))
               (t
                (vector-push-extend (code-char #xfffd) text)
                (incf index)))
          finally (return (values (coerce text 'string) remaining)))))
