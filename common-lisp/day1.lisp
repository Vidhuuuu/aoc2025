#!/usr/bin/env -S sbcl --script

;; part 1
(defun rotate (pos dir amt)
  (case dir
    (#\L (mod (- pos amt) 100))
    (#\R (mod (+ pos amt) 100))
    (otherwise (error "unknown dir: ~A" dir))))

(with-open-file (file (or (sb-ext:posix-getenv "INP")
                          "../input/day1/sample"))
  (let ((pos 50)
        (cnt 0))
    (loop for line = (read-line file nil)
          while line
          do
          (setf pos
                (rotate pos
                        (char line 0)
                        (parse-integer (subseq line 1))))
          (when (zerop pos)
            (incf cnt)))
    (format t "part 1: ~A~%" cnt)))

;; part 2
(defun rotate2 (pos dir amt)
  (let ((d (case dir
             (#\L -1)
             (#\R 1)
             (otherwise (error "unknown dir: ~A" dir))))
        (cnt 0))
    (loop repeat amt
          do
          (setf pos (mod (+ pos d) 100))
          (when (zerop pos)
            (incf cnt)))
    (values pos cnt)))

(with-open-file (file (or (sb-ext:posix-getenv "INP")
                          "../input/day1/sample"))
  (let ((pos 50)
        (cnt 0))
    (loop for line = (read-line file nil)
          while line
          do
          (multiple-value-bind (new-pos n)
            (rotate2 pos
                     (char line 0)
                     (parse-integer (subseq line 1)))
            (setf pos new-pos)
            (setf cnt (+ cnt n))))
    (format t "part 2: ~A~%" cnt)))
