#!/usr/bin/sbcl --script

(load "~/.sbclrc")

(ql:quickload "str")

(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line collect line)))

(defun digitify (str)
  (loop for (stringified with-digit) in '(("one" "one1one")
                                          ("two" "two2two")
                                          ("three" "three3three")
                                          ("four" "four4four")
                                          ("five" "five5five")
                                          ("six" "six6six")
                                          ("seven" "seven7seven")
                                          ("eight" "eight8eight")
                                          ("nine" "nine9nine"))
        do (setf str (str:replace-all stringified with-digit str))
        finally (return str)))

(defun first-last-digit (str)
  (loop for c in (remove-if #'null (map 'list #'digit-char-p str))
        with first = nil
        with last = nil
        if (null first)
          do (setf first c)
        do (setf last c)
        finally (return `(,first ,last))))

(defun make-num (pair)
  (destructuring-bind (tens unities) pair
    (+ (* 10 tens) unities)))

(defun solve (input)
  (reduce #'+
          (mapcar #'make-num
                  (mapcar #'first-last-digit input))))

(defun solve-2 (input)
  (solve (mapcar #'digitify input)))

(format t "Day 1.1 = ~d~%" (solve (read-file "input")))

(format t "Day 1.2 = ~d~%" (solve-2 (read-file "input")))

(defvar *test-input* '("1abc2"
                       "pqr3stu8vwx"
                       "a1b2c3d4e5f"
                       "treb7uchet"))
(assert (= 142 (solve *test-input*)))

(defvar *test-input-2* '("two1nine"
                         "eightwothree"
                         "abcone2threexyz"
                         "xtwone3four"
                         "4nineeightseven2"
                         "zoneight234"
                         "7pqrstsixteen"))
(assert (= 281 (solve-2 *test-input-2*)))
