(in-package #:mtm.terminal)

(defconstant +ascii-bell-code+ 7)

(defstruct (screen-cell
            (:constructor new-screen-cell (character style))
            (:copier nil))
  (character #\Space :type character)
  (style nil :type list))

(defun get-screen-cell-copy (cell)
  "Return an independent copy of CELL."
  (new-screen-cell (screen-cell-character cell)
                   (copy-list (screen-cell-style cell))))

;; Store one retained terminal display and parser state.
(defclass terminal-emulator ()
  ((width
    :initarg :width
    :accessor terminal-width)
   (height
    :initarg :height
    :accessor terminal-height)
   (cells
    :initarg :cells
    :accessor terminal-cells)
   (primary-cells
    :initform nil
    :accessor primary-cells)
   (primary-cursor-row
    :initform 0
    :accessor primary-cursor-row)
   (primary-cursor-column
    :initform 0
    :accessor primary-cursor-column)
   ;; Save styles from the primary display.
   (primary-style
    :initform nil
    :accessor primary-style)
   (primary-saved-row
    :initform 0
    :accessor primary-saved-row)
   (primary-saved-column
    :initform 0
    :accessor primary-saved-column)
   (primary-saved-style
    :initform nil
    :accessor primary-saved-style)
   (alternate-screen-p
    :initform nil
    :accessor terminal-alternate-screen-p)
   (screen-events
    :initform nil
    :accessor terminal-screen-events)
   (cursor-row
    :initform 0
    :accessor cursor-row)
   (cursor-column
    :initform 0
    :accessor cursor-column)
   ;; Track styles for the active display.
   (style
    :initform nil
    :accessor terminal-style)
   (saved-row
    :initform 0
    :accessor saved-row)
   (saved-column
    :initform 0
    :accessor saved-column)
   (saved-style
    :initform nil
    :accessor saved-style)
   (parser-state
    :initform :ground
    :accessor parser-state)
   (csi-buffer
    :initform ""
    :accessor csi-buffer)
   (osc-escape-p
    :initform nil
    :accessor osc-escape-p)))

(defun new-terminal-row (width)
  (make-array width
              :initial-contents
              (loop repeat width
                    collect (new-screen-cell #\Space nil))))

(defun new-terminal-screen (width height)
  (make-array height
              :initial-contents
              (loop repeat height
                    collect (new-terminal-row width))))

(defun set-terminal-screen (cells width height)
  "Resize CELLS while keeping its intersecting display cells."
  (let ((old-height (length cells))
        (old-width (if (plusp (length cells))
                       (length (aref cells 0))
                       0)))
    (make-array
     height
     :initial-contents
     (loop for row-index below height
           collect (make-array
                    width
                    :initial-contents
                    (loop for column below width
                          collect (if (and (< row-index old-height)
                                           (< column old-width))
                                      (get-screen-cell-copy
                                       (aref (aref cells row-index) column))
                                      (new-screen-cell #\Space nil))))))))

(defun get-terminal-screen-copy (cells)
  "Return an independent copy of CELLS."
  (set-terminal-screen cells
                          (length (aref cells 0))
                          (length cells)))

(defun new-terminal-emulator (&key (width 80)
                                    (height 24))
  "Create a terminal emulator with WIDTH columns and HEIGHT rows."
  (check-type width (integer 1))
  (check-type height (integer 1))
  (make-instance 'terminal-emulator
                 :width width
                 :height height
                 :cells (new-terminal-screen width height)))

;; Copy the terminal's retained display state.
(defun get-terminal-copy (terminal)
  "Return an independent copy of TERMINAL's retained screen state."
  (let ((copy (new-terminal-emulator
               :width (terminal-width terminal)
               :height (terminal-height terminal))))
    (setf (terminal-cells copy)
          (get-terminal-screen-copy (terminal-cells terminal))
          (primary-cells copy)
          (and (primary-cells terminal)
               (get-terminal-screen-copy (primary-cells terminal)))
          (primary-cursor-row copy) (primary-cursor-row terminal)
          (primary-cursor-column copy) (primary-cursor-column terminal)
          (primary-style copy) (copy-list (primary-style terminal))
          (primary-saved-row copy) (primary-saved-row terminal)
          (primary-saved-column copy) (primary-saved-column terminal)
          (primary-saved-style copy) (copy-list (primary-saved-style terminal))
          (terminal-alternate-screen-p copy)
          (terminal-alternate-screen-p terminal)
          (cursor-row copy) (cursor-row terminal)
          (cursor-column copy) (cursor-column terminal)
          (terminal-style copy) (copy-list (terminal-style terminal))
          (saved-row copy) (saved-row terminal)
          (saved-column copy) (saved-column terminal)
          (saved-style copy) (copy-list (saved-style terminal))
          (parser-state copy) (parser-state terminal)
          (csi-buffer copy) (copy-seq (csi-buffer terminal))
          (osc-escape-p copy) (osc-escape-p terminal))
    copy))

(defun get-terminal-screen-events (terminal)
  "Return and clear screen mode events from TERMINAL."
  (prog1 (nreverse (terminal-screen-events terminal))
    (setf (terminal-screen-events terminal) nil)))

(defun set-terminal-state (terminal)
  (setf (cursor-row terminal)
        (max 0 (min (cursor-row terminal) (1- (terminal-height terminal))))
        (cursor-column terminal)
        (max 0 (min (cursor-column terminal) (terminal-width terminal)))
        (saved-row terminal)
        (max 0 (min (saved-row terminal) (1- (terminal-height terminal))))
        (saved-column terminal)
        (max 0 (min (saved-column terminal) (terminal-width terminal))))
  terminal)

(defun set-terminal-size (terminal width height)
  "Resize TERMINAL and preserve its intersecting screen cells."
  (check-type width (integer 1))
  (check-type height (integer 1))
  (setf (terminal-cells terminal)
        (set-terminal-screen (terminal-cells terminal) width height)
        (primary-cells terminal)
        (and (primary-cells terminal)
             (set-terminal-screen (primary-cells terminal) width height))
        (terminal-width terminal) width
        (terminal-height terminal) height)
  (set-terminal-state terminal)
  (when (primary-cells terminal)
    (setf (primary-cursor-row terminal)
          (max 0 (min (primary-cursor-row terminal) (1- height)))
          (primary-cursor-column terminal)
          (max 0 (min (primary-cursor-column terminal) width))
          (primary-saved-row terminal)
          (max 0 (min (primary-saved-row terminal) (1- height)))
          (primary-saved-column terminal)
          (max 0 (min (primary-saved-column terminal) width))))
  terminal)

(defun get-terminal-cell (terminal row column)
  "Return a copy of the cell at one-based ROW and COLUMN."
  (check-type row (integer 1))
  (check-type column (integer 1))
  (get-screen-cell-copy
   (aref (aref (terminal-cells terminal) (1- row)) (1- column))))

(defun get-terminal-screen-lines (terminal)
  "Return the terminal screen as full-width strings."
  (loop with width = (terminal-width terminal)
        for row across (terminal-cells terminal)
        collect (let ((line (make-string width)))
                  (loop for column below width
                        for cell = (aref row column)
                        do (setf (char line column)
                                 (screen-cell-character cell)))
                  line)))

(defun get-terminal-cursor-position (terminal)
  "Return the cursor position as one-based row and column values."
  (values (1+ (cursor-row terminal))
          (1+ (min (cursor-column terminal)
                   (1- (terminal-width terminal))))))

(defun get-clamped-value (value minimum maximum)
  (max minimum (min value maximum)))

;; Reset the terminal display and parser state.
(defun set-terminal-reset (terminal)
  (let ((was-alternate-p (terminal-alternate-screen-p terminal)))
    (setf (terminal-cells terminal)
          (new-terminal-screen (terminal-width terminal)
                               (terminal-height terminal))
          (cursor-row terminal) 0
          (cursor-column terminal) 0
          (terminal-style terminal) nil
          (saved-row terminal) 0
          (saved-column terminal) 0
          (saved-style terminal) nil
          (primary-cells terminal) nil
          (primary-cursor-row terminal) 0
          (primary-cursor-column terminal) 0
          (primary-style terminal) nil
          (primary-saved-row terminal) 0
          (primary-saved-column terminal) 0
          (primary-saved-style terminal) nil
          (terminal-alternate-screen-p terminal) nil
          (terminal-screen-events terminal)
          (if was-alternate-p
              (cons :leave (terminal-screen-events terminal))
              nil)
          (parser-state terminal) :ground
          (csi-buffer terminal) ""
          (osc-escape-p terminal) nil))
  terminal)

;; Enter the alternate terminal display.
(defun set-terminal-alternate-screen (terminal)
  (unless (terminal-alternate-screen-p terminal)
    (setf (primary-cells terminal) (terminal-cells terminal)
          (primary-cursor-row terminal) (cursor-row terminal)
          (primary-cursor-column terminal) (cursor-column terminal)
          (primary-style terminal) (copy-list (terminal-style terminal))
          (primary-saved-row terminal) (saved-row terminal)
          (primary-saved-column terminal) (saved-column terminal)
          (primary-saved-style terminal) (copy-list (saved-style terminal))
          (terminal-cells terminal)
          (new-terminal-screen (terminal-width terminal)
                               (terminal-height terminal))
          (cursor-row terminal) 0
          (cursor-column terminal) 0
          (terminal-style terminal) nil
          (saved-row terminal) 0
          (saved-column terminal) 0
          (saved-style terminal) nil
          (terminal-alternate-screen-p terminal) t
          (terminal-screen-events terminal)
          (cons :enter (terminal-screen-events terminal))))
  terminal)

;; Restore the primary terminal display.
(defun del-terminal-alternate-screen (terminal)
  (when (terminal-alternate-screen-p terminal)
    (setf (terminal-cells terminal) (primary-cells terminal)
          (cursor-row terminal) (primary-cursor-row terminal)
          (cursor-column terminal) (primary-cursor-column terminal)
          (terminal-style terminal) (copy-list (primary-style terminal))
          (saved-row terminal) (primary-saved-row terminal)
          (saved-column terminal) (primary-saved-column terminal)
          (saved-style terminal) (copy-list (primary-saved-style terminal))
          (primary-cells terminal) nil
          (terminal-alternate-screen-p terminal) nil
          (terminal-screen-events terminal)
          (cons :leave (terminal-screen-events terminal))))
  terminal)

(defun set-terminal-scroll (terminal)
  (let ((cells (terminal-cells terminal))
        (height (terminal-height terminal)))
    (loop for row below (1- height)
          do (setf (aref cells row) (aref cells (1+ row))))
    (setf (aref cells (1- height))
          (new-terminal-row (terminal-width terminal)))))

(defun set-terminal-line-feed (terminal)
  (if (= (cursor-row terminal) (1- (terminal-height terminal)))
      (set-terminal-scroll terminal)
      (incf (cursor-row terminal))))

;; Write CHARACTER at the terminal cursor.
(defun set-terminal-character (terminal character)
  (when (>= (cursor-column terminal) (terminal-width terminal))
    (setf (cursor-column terminal) 0)
    (set-terminal-line-feed terminal))
  (let ((cell (aref (aref (terminal-cells terminal) (cursor-row terminal))
                    (cursor-column terminal))))
    (setf (screen-cell-character cell) character
          (screen-cell-style cell) (copy-list (terminal-style terminal))))
  (incf (cursor-column terminal)))

(defun set-terminal-cursor (terminal row column)
  (setf (cursor-row terminal)
        (get-clamped-value row 0 (1- (terminal-height terminal)))
        (cursor-column terminal)
        (get-clamped-value column 0 (terminal-width terminal))))

(defun del-terminal-cell (terminal row column)
  (setf (aref (aref (terminal-cells terminal) row) column)
        (new-screen-cell #\Space nil)))

(defun del-terminal-line (terminal mode)
  (let* ((row (cursor-row terminal))
         (start (if (= mode 1) 0 (cursor-column terminal)))
         (end (if (= mode 1)
                  (min (cursor-column terminal) (1- (terminal-width terminal)))
                  (1- (terminal-width terminal)))))
    (when (= mode 2)
      (setf start 0 end (1- (terminal-width terminal))))
    (loop for column from start to end
          do (del-terminal-cell terminal row column))))

(defun del-terminal-display (terminal mode)
  (let ((row (cursor-row terminal))
        (column (cursor-column terminal))
        (height (terminal-height terminal))
        (width (terminal-width terminal)))
    (cond
      ((= mode 2)
       (loop for screen-row below height
             do (loop for screen-column below width
                      do (del-terminal-cell terminal screen-row screen-column))))
      ((= mode 1)
       (loop for screen-row from 0 to row
             do (loop for screen-column below width
                      when (or (< screen-row row)
                               (<= screen-column column))
                        do (del-terminal-cell terminal screen-row screen-column))))
      (t
       (loop for screen-row from row below height
             do (loop for screen-column below width
                      when (or (> screen-row row)
                               (>= screen-column column))
                        do (del-terminal-cell terminal screen-row screen-column)))))))

(defun get-csi-default-parameter (parameters index default)
  (let ((value (nth index parameters)))
    (if (or (null value) (zerop value)) default value)))

(defun get-csi-parameters (buffer)
  (let* ((private (and (plusp (length buffer))
                       (member (char buffer 0) '(#\? #\> #\< #\=))))
         (start (if private 1 0))
         (values nil)
         (field-start start))
    (loop for index from start to (length buffer)
          when (or (= index (length buffer))
                   (char= (char buffer index) #\;))
            do (push (unless (= field-start index)
                       (parse-integer buffer :start field-start :end index))
                     values)
               (setf field-start (1+ index)))
    (values (nreverse values) private)))

(defun get-style-kind (code)
  ;; ANSI SGR assigns these ranges to styles and colors.
  (cond
    ((member code '(1 2 3 4)) :decoration)
    ((or (<= 30 code 37) (<= 90 code 97)) :foreground)
    ((or (<= 40 code 47) (<= 100 code 107)) :background)
    (t nil)))

;; Apply one ANSI style CODE to the terminal state.
(defun set-terminal-style-code (terminal code)
  (cond
    ((= code 0) (setf (terminal-style terminal) nil))
    ((member code '(22 23 24))
     (setf (terminal-style terminal)
           (remove-if (lambda (item)
                        (member item (case code
                                       (22 '(1 2))
                                       (23 '(3))
                                       (24 '(4)))))
                      (terminal-style terminal))))
    ((member code '(39 49))
     (setf (terminal-style terminal)
           (remove-if (lambda (item)
                        (eq (get-style-kind item)
                            (if (= code 39) :foreground :background)))
                      (terminal-style terminal))))
    (t
     (let ((kind (get-style-kind code)))
       (when kind
         (setf (terminal-style terminal)
               (remove-if (lambda (item)
                            (eq (get-style-kind item) kind))
                          (terminal-style terminal))))
       (pushnew code (terminal-style terminal) :test #'eql)))))

(defun set-terminal-sgr (terminal parameters)
  (dolist (code (or parameters '(0)))
    (set-terminal-style-code terminal (or code 0))))

;; Apply one parsed ANSI CSI sequence.
(defun set-terminal-csi (terminal final)
  (multiple-value-bind (parameters private)
      (get-csi-parameters (csi-buffer terminal))
    (cond
      ((and private
            (member final '(#\h #\l))
            (intersection parameters '(47 1047 1049)))
       (if (char= final #\h)
           (set-terminal-alternate-screen terminal)
           (del-terminal-alternate-screen terminal)))
      (t
       (case final
         (#\A (set-terminal-cursor terminal
                                    (- (cursor-row terminal)
                                       (get-csi-default-parameter parameters 0 1))
                                    (cursor-column terminal)))
         (#\B (set-terminal-cursor terminal
                                    (+ (cursor-row terminal)
                                       (get-csi-default-parameter parameters 0 1))
                                    (cursor-column terminal)))
         (#\C (set-terminal-cursor terminal
                                    (cursor-row terminal)
                                    (+ (cursor-column terminal)
                                       (get-csi-default-parameter parameters 0 1))))
         (#\D (set-terminal-cursor terminal
                                    (cursor-row terminal)
                                    (- (cursor-column terminal)
                                       (get-csi-default-parameter parameters 0 1))))
         (#\E (set-terminal-cursor terminal
                                    (+ (cursor-row terminal)
                                       (get-csi-default-parameter parameters 0 1))
                                    0))
         (#\F (set-terminal-cursor terminal
                                    (- (cursor-row terminal)
                                       (get-csi-default-parameter parameters 0 1))
                                    0))
         ((#\G #\`) (set-terminal-cursor terminal
                      (cursor-row terminal)
                      (1- (get-csi-default-parameter parameters 0 1))))
         (#\d (set-terminal-cursor terminal
                                    (1- (get-csi-default-parameter parameters 0 1))
                                    (cursor-column terminal)))
         ((#\H #\f) (set-terminal-cursor terminal
                      (1- (get-csi-default-parameter parameters 0 1))
                      (1- (get-csi-default-parameter parameters 1 1))))
         (#\J (del-terminal-display terminal (or (first parameters) 0)))
         (#\K (del-terminal-line terminal (or (first parameters) 0)))
         (#\m (set-terminal-sgr terminal parameters))
         (#\s (setf (saved-row terminal) (cursor-row terminal)
                     (saved-column terminal) (cursor-column terminal)
                     (saved-style terminal) (copy-list (terminal-style terminal))))
         (#\u (set-terminal-cursor terminal (saved-row terminal) (saved-column terminal))
               (setf (terminal-style terminal) (copy-list (saved-style terminal))))
         ;; CSI c queries device attributes. It does not reset the screen.
         (#\c nil))))))

;; Apply one escaped terminal CHARACTER.
(defun set-terminal-escape-character (terminal character)
  (case character
    (#\[ (setf (parser-state terminal) :csi
                (csi-buffer terminal) ""))
    (#\] (setf (parser-state terminal) :osc
                (osc-escape-p terminal) nil))
    (#\7 (setf (saved-row terminal) (cursor-row terminal)
                (saved-column terminal) (cursor-column terminal)
                (saved-style terminal) (copy-list (terminal-style terminal))
                (parser-state terminal) :ground))
    (#\8 (set-terminal-cursor terminal (saved-row terminal) (saved-column terminal))
          (setf (terminal-style terminal) (copy-list (saved-style terminal))
                (parser-state terminal) :ground))
    (#\c (set-terminal-reset terminal))
    (#\D (set-terminal-line-feed terminal)
          (setf (parser-state terminal) :ground))
    (#\E (setf (cursor-column terminal) 0)
          (set-terminal-line-feed terminal)
          (setf (parser-state terminal) :ground))
    (#\M (if (plusp (cursor-row terminal))
              (decf (cursor-row terminal)))
          (setf (parser-state terminal) :ground))
    ((#\( #\)) (setf (parser-state terminal) :escape-intermediate))
    (otherwise (setf (parser-state terminal) :ground))))

(defun set-terminal-csi-character (terminal character)
  (let ((code (char-code character)))
    (cond
      ((and (<= #x40 code) (<= code #x7e))
       (set-terminal-csi terminal character)
       (setf (parser-state terminal) :ground
             (csi-buffer terminal) ""))
      ((or (<= #x30 code #x3f)
           (and (zerop (length (csi-buffer terminal)))
                (member character '(#\? #\> #\< #\=))))
       (setf (csi-buffer terminal)
             (concatenate 'string (csi-buffer terminal)
                          (string character))))
      ((<= #x20 code #x2f) nil)
      (t (setf (parser-state terminal) :ground
               (csi-buffer terminal) "")))))

(defun set-terminal-input-character (terminal character)
  (case (parser-state terminal)
    (:ground
     (case character
       (#\Escape (setf (parser-state terminal) :escape))
       (#\Newline (set-terminal-line-feed terminal))
       (#\Return (setf (cursor-column terminal) 0))
       (#\Backspace (setf (cursor-column terminal)
                           (max 0 (1- (cursor-column terminal)))))
       (#\Tab (setf (cursor-column terminal)
                     (min (terminal-width terminal)
                          (* 8 (1+ (floor (cursor-column terminal) 8))))))
       (otherwise
        (unless (= (char-code character) +ascii-bell-code+)
          (when (>= (char-code character) #x20)
            (set-terminal-character terminal character))))))
    (:escape (set-terminal-escape-character terminal character))
    (:escape-intermediate (setf (parser-state terminal) :ground))
    (:csi (set-terminal-csi-character terminal character))
    (:osc
     (cond
       ;; SBCL names Unicode U+1F514 "Bell".
       ((= (char-code character) +ascii-bell-code+)
        (setf (parser-state terminal) :ground))
       ((char= character #\Escape)
        (setf (parser-state terminal) :osc-escape))))
    (:osc-escape
     (if (char= character #\\)
         (setf (parser-state terminal) :ground)
         (setf (parser-state terminal) :osc)))))

(defun set-terminal-input (terminal text)
  "Feed decoded UTF-8 terminal text into TERMINAL."
  (check-type text string)
  (loop for character across text
        do (set-terminal-input-character terminal character))
  terminal)

(defun set-terminal-style (stream style)
  (write-string (format nil "~C[~{~A~^;~}m" #\Escape style) stream))

;; Render the retained terminal display as ANSI text.
(defun get-terminal-render (terminal)
  "Return the screen as ANSI text for the terminal."
  (with-output-to-string (stream)
    (write-string (format nil "~C[H~C[2J" #\Escape #\Escape) stream)
    (loop for row across (terminal-cells terminal)
          for row-index below (terminal-height terminal)
          do (let ((style nil))
               (loop for cell across row
                     for cell-style = (screen-cell-style cell)
                     do (unless (equal style cell-style)
                          (if cell-style
                              (set-terminal-style stream cell-style)
                              (set-terminal-style stream '(0)))
                          (setf style cell-style))
                        (write-char (screen-cell-character cell) stream))
               (when style
                 (set-terminal-style stream '(0)))
               (when (< row-index (1- (terminal-height terminal)))
                 (write-string (format nil "~C~C" #\Return #\Newline)
                               stream))))
    (format stream "~C[~D;~DH"
            #\Escape
            (1+ (cursor-row terminal))
            (1+ (min (cursor-column terminal)
                     (1- (terminal-width terminal)))))))
