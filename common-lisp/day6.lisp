#!/usr/bin/env -S sbcl --script

(defun gather (line &key sep)
  (let ((parts '())
        (cur '()))
    (loop for c across line
          do
          (if (char= c sep)
            (when cur
              (push (coerce (reverse cur) 'string) parts)
              (setf cur '()))
            (push c cur)))
    (when cur
      (push (coerce (reverse cur) 'string) parts))
    (reverse parts)))

(defun apply-op (op operands)
  (cond
    ((string= op "*") (apply #'* (mapcar #'parse-integer operands)))
    ((string= op "+") (apply #'+ (mapcar #'parse-integer operands)))
    (t (error "unknown operand: ~A~%" op))))

(defun calculate (operand-lines ops)
  (labels ((aux (operand-lines ops total)
                (if (null ops)
                  total 
                  (let ((op (first ops))
                        (operands (mapcar #'first operand-lines)))
                    (aux (mapcar #'rest operand-lines)
                         (rest ops)
                         (+ total (apply-op op operands)))))))
    (aux operand-lines ops 0)))

(defun column (lines i)
  (mapcar (lambda (line)
            (char line i)) lines))

(defun calculate2 (operand-lines ops)
  ; (let ((lst '("foo" "bar" "baz")))
  ;   (loop for i downfrom (1- (length (first lst))) to 0
  ;         do
  ;         (format t "~A~%" (column lst i)))))
  (assert (apply #'= (mapcar #'length operand-lines)))
  (let ((transposed '()))
    (loop for i downfrom (1- (length (first operand-lines))) to 0
          do
          (push (column operand-lines i) transposed))
    (setf transposed (reverse transposed))
    ; (dolist (r transposed)
    ;   (format t "~A~%" (coerce r 'string)))))
    (labels ((aux (transposed operands ops total)
                  (if (null transposed)
                    (+ total (apply-op (first ops) operands))
                    (let ((cur (first transposed)))
                      (if (every (lambda (c)
                                   (char= c #\Space)) cur)
                        (let ((op (first ops)))
                          (aux (rest transposed)
                               '()
                               (rest ops)
                               (+ total (apply-op op operands))))
                        (aux (rest transposed)
                             (push (coerce cur 'string) operands)
                             ops
                             total))))))
      (aux transposed '() ops 0))))

(with-open-file (file (or (sb-ext:posix-getenv "INP")
                          "../input/day6/sample"))
  (let ((raw-operand-lines '())
        (operand-lines '())
        (ops '()))
    (loop for line = (read-line file nil)
          while line
          do
          (let ((parts (gather line :sep #\Space)))
            (if (member "*" parts :test #'string=)
              (setf ops parts)
              (progn
                (push parts operand-lines)
                (push line raw-operand-lines)))))
    (setf raw-operand-lines (reverse raw-operand-lines))

    (dolist (operand-line operand-lines)
      (assert (= (length operand-line)
                 (length ops))))

    (format t "part 1: ~A~%" (calculate operand-lines ops))
    (format t "part 2: ~A~%" (calculate2 raw-operand-lines (reverse ops)))))
