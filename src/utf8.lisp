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
  (let* (;; Combine pending bytes with the current PTY chunk.
         (input (if pending
                    (concatenate '(vector (unsigned-byte 8)) pending bytes)
                    bytes))
         ;; Track the next undecoded input position.
         (index 0)
         ;; Cache the combined input length.
         (end (length input))
         ;; Store incomplete trailing bytes for the next chunk.
         (remaining nil))
    (values
     ;; Collect decoded text without writing another decoder.
     (with-output-to-string (text)
       (loop while (< index end)
             do (handler-case
                    (progn
                      (write-string
                       (babel:octets-to-string
                        input
                        :start index
                        :end end
                        :encoding :utf-8
                        :errorp t)
                       text)
                      (setf index end))
                  (babel:end-of-input-in-character (condition)
                    ;; Read the first incomplete character position.
                    (let ((position
                            (babel:character-coding-error-position condition)))
                      (if flush-p
                          (progn
                            (write-string
                             (babel:octets-to-string
                              input
                              :start position
                              :end end
                              :encoding :utf-8
                              :errorp nil)
                             text)
                            (setf index end))
                          (setf remaining (subseq input position)
                                index end))))
                  (babel:character-decoding-error (condition)
                    ;; Read the first malformed character position.
                    (let* ((position
                             (babel:character-coding-error-position condition))
                           ;; Let Babel locate the malformed character end.
                           (next
                             (nth-value
                              1
                              (babel:vector-size-in-chars
                               input
                               :start position
                               :end end
                               :max 1
                               :errorp nil
                               :encoding :utf-8))))
                      (write-string
                       (babel:octets-to-string
                        input
                        :start index
                        :end position
                        :encoding :utf-8
                        :errorp nil)
                       text)
                      (write-string
                       (babel:octets-to-string
                        input
                        :start position
                        :end next
                        :encoding :utf-8
                        :errorp nil)
                       text)
                      (setf index next))))))
     remaining)))

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
