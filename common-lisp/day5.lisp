#!/usr/bin/env -S sbcl --script

(defun split-first (s &key delim)
  (let ((pos (position delim s)))
    (if pos
      (values (subseq s 0 pos)
              (subseq s (1+ pos)))
      (values s nil))))

(defun process-range (range-str)
  (multiple-value-bind
    (lb ub)
    (split-first range-str :delim #\-)
    (values (parse-integer lb)
            (parse-integer ub))))

(defun merge-ranges (ranges)
  (labels ((aux (lb ub l acc)
                (if (null l)
                  (push (list lb ub) acc)
                  (let* ((next (first l))
                         (cur-lb (first next))
                         (cur-ub (second next)))
                    (if (<= cur-lb ub)
                      (aux lb (max ub cur-ub) (rest l) acc)
                      (aux
                        cur-lb
                        cur-ub
                        (rest l)
                        (push (list lb ub) acc)))))))
    (aux (first (first ranges))
         (second (first ranges))
         ranges
         '())))

(with-open-file (file (or (sb-ext:posix-getenv "INP")
                          "../input/day5/sample"))
  (let ((ranges '())
        (ids '())
        (still-ranges t))
    (loop for line = (read-line file nil)
          while line
          do
          (if (string= line "")
            (setf still-ranges nil)
            (if still-ranges
              (multiple-value-bind
                (lb ub)
                (process-range line)
                (push (list lb ub) ranges))
              (push (parse-integer line) ids))))
    (let ((fresh-count 0)
          (total-fresh-count 0))
      (dolist (id ids)
        (dolist (range ranges)
          (let ((lb (first range))
                (ub (second range)))
            (when (and (>= id lb)
                       (<= id ub))
              (incf fresh-count)
              (return)))))
      (let* ((sorted-ranges (sort ranges
                                  (lambda (a b)
                                    (< (first a) (first b)))))
             (merged-ranges (merge-ranges sorted-ranges)))
        ; (format t "~A~%" sorted-ranges)
        ; (format t "~A~%" merged-ranges)
        (dolist (range merged-ranges)
          (setf total-fresh-count (+ total-fresh-count
                                     (1+ (- (second range)
                                            (first range))))))
        (format t "part 1: ~A~%" fresh-count)
        (format t "part 2: ~A~%" total-fresh-count)))))
