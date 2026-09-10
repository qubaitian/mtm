(defpackage #:mtm.test.terminal
  (:use #:cl)
  (:import-from #:mtm.test
                #:check
                #:deftest
                #:screen-has-text-p)
  (:import-from #:mtm.terminal
                #:get-terminal-cell
                #:get-terminal-cursor-position
                #:get-terminal-render
                #:get-terminal-screen-lines
                #:new-terminal-emulator
                #:screen-cell-character
                #:screen-cell-style
                #:set-terminal-input
                #:terminal-alternate-screen-p))

(in-package #:mtm.test.terminal)

(deftest terminal-projects-ansi-output ()
  (let ((terminal (new-terminal-emulator :width 8 :height 2)))
    (set-terminal-input terminal (format nil "abc~C[2J~C[Hxy" #\Escape #\Escape))
    (check (string= "xy      " (first (get-terminal-screen-lines terminal)))
           "ANSI cursor or erase handling is incorrect.")
    (check (not (search "mtm" (get-terminal-render terminal)))
           "The terminal projection contains unexpected interface text.")
    (multiple-value-bind (row column)
        (get-terminal-cursor-position terminal)
      (check (and (= row 1) (= column 3))
             "The terminal cursor position is incorrect."))))

(deftest terminal-preserves-styles ()
  (let ((terminal (new-terminal-emulator :width 8 :height 2)))
    (set-terminal-input terminal (format nil "~C[31mred~C[0mplain"
                                         #\Escape #\Escape))
    (let ((styled (get-terminal-cell terminal 1 1))
          (plain (get-terminal-cell terminal 1 4)))
      (check (and (char= #\r (screen-cell-character styled))
                  (member 31 (screen-cell-style styled)))
             "The terminal projection loses SGR style.")
      (check (null (screen-cell-style plain))
             "The terminal projection keeps cleared SGR style."))
    (check (equal '("redplain" "        ")
                   (get-terminal-screen-lines terminal))
           "The terminal projection loses screen content.")))

(deftest terminal-restores-primary-screen-after-alternate-screen ()
  (let ((terminal (new-terminal-emulator :width 8 :height 2)))
    (set-terminal-input terminal "main")
    (set-terminal-input terminal (format nil "~C[?1049hvim" #\Escape))
    (check (terminal-alternate-screen-p terminal)
           "The terminal does not enter its alternate screen.")
    (check (not (screen-has-text-p terminal "main"))
           "The alternate screen keeps primary screen text.")
    (set-terminal-input terminal (format nil "~C[?1049l" #\Escape))
    (check (not (terminal-alternate-screen-p terminal))
           "The terminal does not return from its alternate screen.")
    (check (screen-has-text-p terminal "main")
           "The terminal loses primary screen text on return.")))

