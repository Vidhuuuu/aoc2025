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

(defun area (p1 p2)
  (* (1+ (abs (- (first p1) (first p2))))
     (1+ (abs (- (second p1) (second p2))))))

(with-open-file (file (or (sb-ext:posix-getenv "INP")
                          "../input/day9/sample"))
  (let ((red-tiles
          (loop for line = (read-line file nil)
                while line
                collect (mapcar #'parse-integer
                                (split line :sep #\,)))))
    (let ((max-area 0))
      (loop for (a . others) on red-tiles
            do
            (loop for b in others
                  do (setf max-area (max max-area (area a b)))))
      (format t "part 1: ~A~%" max-area))
    (let ((max-enclosed-area 0)
          (n (length red-tiles)))
      (labels ((valid-rectangle (p1 p2)
                                (let ((min-x (min (first p1) (first p2)))
                                      (max-x (max (first p1) (first p2)))
                                      (min-y (min (second p1) (second p2)))
                                      (max-y (max (second p1) (second p2))))
                                  (not
                                    (loop for i from 0 below n
                                          for a = (nth i red-tiles)
                                          for b = (nth (mod (1+ i) n) red-tiles)
                                          for edge-min-x = (min (first a) (first b))
                                          for edge-max-x = (max (first a) (first b))
                                          for edge-min-y = (min (second a) (second b))
                                          for edge-max-y = (max (second a) (second b))
                                          thereis
                                          (and (>= edge-max-x (1+ min-x))
                                               (<= edge-min-x (1- max-x))
                                               (>= edge-max-y (1+ min-y))
                                               (<= edge-min-y (1- max-y))))))))
        (loop for (a . others) on red-tiles
              do
              (loop for b in others
                    for curr-area = (area a b)
                    when (> curr-area max-enclosed-area)
                    do
                    (when (valid-rectangle a b)
                      (setf max-enclosed-area curr-area)))))
      (format t "part 2: ~A~%" max-enclosed-area))))
