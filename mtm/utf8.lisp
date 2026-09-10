(defpackage #:mtm.utf8
  (:use #:cl)
  (:export
   #:get-utf8-chunk
   #:get-utf8
   #:get-utf8-character-length
   #:get-utf8-previous-character-start))

(in-package #:mtm.utf8)

;; Encode TEXT through Babel's UTF-8 implementation.
(defun get-utf8 (text)
  "Encode TEXT as a UTF-8 octet vector."
  (babel:string-to-octets text :encoding :utf-8 :errorp nil))

;; Decode BYTES through Babel while preserving incomplete trailing bytes.
(defun get-utf8-chunk (bytes &optional pending flush-p)
  "Decode BYTES and return text plus incomplete trailing bytes.
FLUSH-P decodes an incomplete trailing sequence as replacement text."
  (check-type bytes (vector (unsigned-byte 8)))
  (when pending
    (check-type pending (vector (unsigned-byte 8))))
  (let* ((input (if pending
                    (concatenate '(vector (unsigned-byte 8)) pending bytes)
                    bytes))
         (end (length input))
         ;; Babel's UTF-8 counter locates incomplete tails without decoding.
         (complete-end
           (if flush-p
               end
               (handler-case
                   (progn
                     (babel:vector-size-in-chars
                      input :encoding :utf-8 :errorp t)
                     end)
                 (babel:end-of-input-in-character (condition)
                   (babel:character-coding-error-position condition))))))
    (values
     (babel:octets-to-string
      input :end complete-end :encoding :utf-8 :errorp nil)
     (when (< complete-end end)
       (subseq input complete-end)))))


;; Return the byte length of the first UTF-8 character.
(defun get-utf8-character-length (bytes start &optional end)
  ;; Read Babel's character count and next byte position.
  (multiple-value-bind (count next)
      (babel:vector-size-in-chars bytes
                                  :start start
                                  :end end
                                  :max 1
                                  :errorp nil
                                  :encoding :utf-8)
    (when (plusp count)
      (- next start))))

;; Return the byte start of the character before END.
(defun get-utf8-previous-character-start (bytes end)
  (when (plusp end)
    (let ((count
            (nth-value
             0
             (babel:vector-size-in-chars bytes
                                         :end end
                                         :errorp nil
                                         :encoding :utf-8))))
      (if (= count 1)
          0
          (nth-value
           1
           (babel:vector-size-in-chars bytes
                                       :end end
                                       :max (1- count)
                                       :errorp nil
                                       :encoding :utf-8))))))
