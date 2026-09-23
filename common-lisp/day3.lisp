#!/usr/bin/env -S sbcl --script

(defun explode (s)
  (labels ((aux (i)
                (if (= i (length s))
                  '()
                  (cons (char s i) (aux (1+ i))))))
    (aux 0)))
;; (format t "~S~%" (explode "hello!"))

(defun max-pos (l)
  (let ((mv (first l)) ; max-val
        (mp 0)         ; max-pos
        (pos 0))
    (dolist (x (rest l) (values mv mp))
      (incf pos)
      (when (> x mv)
        (setf mv x
              mp pos)))))

(defun max-joltage (bank)
  (multiple-value-bind (tens pos)
    (max-pos (subseq bank 0 (1- (length bank))))
    (let ((ones (apply #'max (subseq bank (1+ pos)))))
      (+ (* tens 10) ones))))

(defun max-joltage2 (bank)
  (labels ((aux (acc start n)
                (if (zerop n)
                  acc
                  (multiple-value-bind (mv mp)
                    (max-pos (subseq bank start (1+ (- (length bank) n))))
                    (aux (+ (* acc 10) mv)
                         (+ start mp 1) ; mp is relative to start
                         (1- n))))))
    (aux 0 0 12)))

(with-open-file (file (or (sb-ext:posix-getenv "INP")
                          "../input/day3/sample"))
  (let ((total 0))
    (loop for line = (read-line file nil)
            while line 
            do
            (let ((bank (mapcar (lambda (x) (digit-char-p x)) (explode line))))
              ; (setf total (+ total (max-joltage bank)))))
              (setf total (+ total (max-joltage2 bank)))))
    ; (format t "part 1: ~A~%" total)))
    (format t "part 2: ~A~%" total)))
