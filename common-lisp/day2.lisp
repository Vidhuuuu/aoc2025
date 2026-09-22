#!/usr/bin/env -S sbcl --script

(defun split (str &key sep)
  (let ((parts '())
        (start 0))
    (loop for i from 0 below (length str)
          when (char= (char str i) sep)
            do
            (push (subseq str start i) parts)
            (setf start (1+ i))
          finally
            (push (subseq str start) parts))
    (nreverse parts)))

(defun invalidp (n)
  (let* ((n-str (format nil "~D" n))
        (len (length n-str)))
    (and (evenp len)
         (let ((half (/ len 2)))
           (string= (subseq n-str 0 half)
                    (subseq n-str half))))))

(defun invalidp2 (n)
  (let* ((n-str (format nil "~D" n))
         (len (length n-str)))
    (loop for i from 1 to (floor len 2)
          when (zerop (mod len i))
          do
            (let ((prefix (subseq n-str 0 i)))
              (when (loop for j from i below len by i
                          always (string= prefix n-str
                                          :start2 j
                                          :end2 (+ j i)))
                (return t)))
          finally
            (return nil))))

(defun sum-invalid (lb ub)
  (let ((sum 0))
    (loop for i from lb to ub
            do
            (when (invalidp i)
            ; (when (invalidp2 i)
              (setf sum (+ sum i))))
    sum))

(with-open-file (file (or (sb-ext:posix-getenv "INP")
                          "../input/day2/sample"))
  (let ((line (read-line file nil))
        (total 0))
    (dolist (range (split line :sep #\,))
      (let* ((bounds (split range :sep #\-))
             (lb (parse-integer (first bounds)))
             (ub (parse-integer (second bounds))))
        (setf total (+ total (sum-invalid lb ub)))))
    (format t "part 1: ~A~%" total)))
    ; (format t "part 2: ~A~%" total)))
