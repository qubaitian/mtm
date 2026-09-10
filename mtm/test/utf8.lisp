(defpackage #:mtm.test.utf8
  (:use #:cl)
  (:import-from #:mtm.test
                #:check
                #:deftest
                #:new-test-octets)
  (:import-from #:mtm.utf8
                #:get-utf8
                #:get-utf8-chunk))

(in-package #:mtm.test.utf8)

(deftest utf8-encodes-unicode-text ()
  (check (equalp (get-utf8 "★𐀀")
                 (new-test-octets #xe2 #x98 #x85 #xf0 #x90 #x80 #x80))
         "Babel must encode Unicode text as UTF-8."))

(deftest utf8-preserves-split-character ()
  (multiple-value-bind (text pending)
      (get-utf8-chunk (new-test-octets #xe2))
    (check (string= text "")
           "UTF-8 decoding must buffer incomplete characters.")
    (check (equalp pending (new-test-octets #xe2))
           "UTF-8 decoding must return incomplete trailing bytes.")
    (multiple-value-bind (text remaining)
        (get-utf8-chunk (new-test-octets #x98 #x85) pending)
      (check (string= text "★")
             "UTF-8 decoding must join split character bytes.")
      (check (null remaining)
             "UTF-8 decoding must clear completed trailing bytes."))))

(deftest utf8-preserves-prefix-before-split-character ()
  (let ((bytes (new-test-octets 97 98 99 #xe2)))
    (multiple-value-bind (text pending)
        (get-utf8-chunk bytes)
      (check (string= text "abc")
             "UTF-8 decoding must preserve the complete prefix.")
      (check (equalp pending (new-test-octets #xe2))
             "UTF-8 decoding must retain the incomplete suffix."))
    (multiple-value-bind (text pending)
        (get-utf8-chunk bytes nil t)
      (check (string= text
                     (concatenate 'string "abc" (string (code-char #xfffd))))
             "UTF-8 flushing must preserve the complete prefix.")
      (check (null pending)
             "UTF-8 flushing must clear the incomplete suffix."))))

(deftest utf8-replaces-malformed-character ()
  (multiple-value-bind (text pending)
      (get-utf8-chunk (new-test-octets #xe4 65))
    (check (string= text (concatenate 'string (string (code-char #xfffd)) "A"))
           "UTF-8 decoding must replace malformed character bytes.")
    (check (null pending)
           "Malformed UTF-8 must not remain pending.")))

(deftest utf8-flushes-incomplete-character ()
  (multiple-value-bind (text pending)
      (get-utf8-chunk (new-test-octets #xe4))
    (declare (ignore text))
    (multiple-value-bind (flushed remaining)
        (get-utf8-chunk pending nil t)
      (check (string= flushed (string (code-char #xfffd)))
             "UTF-8 termination must replace incomplete characters.")
      (check (null remaining)
             "UTF-8 termination must clear pending bytes."))))

