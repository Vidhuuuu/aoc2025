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

(defun take (n arr)
  (loop for i below n
        collect (aref arr i)))

(defstruct coord
  x
  y
  z)

(defun dist (a b)
  (sqrt (+ (expt (- (coord-x b) (coord-x a)) 2)
           (expt (- (coord-y b) (coord-y a)) 2)
           (expt (- (coord-z b) (coord-z a)) 2))))

(defstruct (dsu (:constructor %make-dsu (parent size components)))
  parent
  size
  components)

(defun make-dsu (n)
  (let ((parent (make-array n))
        (size (make-array n :initial-element 1)))
    (dotimes (i n)
      (setf (aref parent i) i))
    (%make-dsu parent size n)))

(defun find-set (d v)
  (let ((parent (dsu-parent d)))
    (if (= v (aref parent v))
      v
      (setf (aref parent v)
            (find-set d (aref parent v))))))

(defun union-sets (d a b)
  (let ((a (find-set d a))
        (b (find-set d b))
        (parent (dsu-parent d))
        (size (dsu-size d)))
    (unless (= a b)
      (when (< (aref size a)
               (aref size b))
        (rotatef a b))
      (setf (aref parent b) a)
      (incf (aref size a)
            (aref size b))
      (decf (dsu-components d)))))

(with-open-file (file (or (sb-ext:posix-getenv "INP")
                          "../input/day8/sample"))
  (let* ((boxes
           (loop for line = (read-line file nil)
                 while line
                 collect (let ((parts (split line :sep #\,)))
                           (assert (= (length parts) 3))
                           (make-coord :x (parse-integer (first parts))
                                       :y (parse-integer (second parts))
                                       :z (parse-integer (third parts))))))
         (edges
           (sort (loop for i from 0 below (1- (length boxes))
                       append
                       (loop for j from (1+ i) below (length boxes)
                             collect
                             (list i j (dist (nth i boxes)
                                             (nth j boxes)))))
                 (lambda (a b) (< (third a) (third b))))))
    (format t "part 1: ~A~%"
            (let ((d (make-dsu (length boxes))))
              ; (loop for i from 0 below 10
              (loop for i from 0 below 1000
                    do
                    ; (format t "on edge=~A, that is ~A~%" i (nth i edges))
                    (destructuring-bind (a b _) (nth i edges)
                      (union-sets d a b)))
              ; (print (type-of (sort (dsu-size d) #'>)))
              (apply #'* (take 3 (sort (dsu-size d) #'>)))))
    (format t "part 2: ~A~%"
            (let ((d (make-dsu (length boxes)))
                  (last-edge nil))
              (loop for edge in edges
                    while (> (dsu-components d) 1)
                    do
                    (when (union-sets d (first edge) (second edge))
                      (setf last-edge edge)))
              (* (coord-x (nth (first last-edge) boxes))
                 (coord-x (nth (second last-edge) boxes)))))))
