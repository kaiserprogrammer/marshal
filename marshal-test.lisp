(in-package :marshal)

(def-suite marshal)
(in-suite marshal)

(defclass test-object () ())

(test marshaling-simple-objects
  (is (string= "string"
               (load (dump "string"))))
  (is (= 1
         (load (dump 1))))
  (is (= 1.0
         (load (dump 1.0))))
  (is (= 1/3
         (load (dump 1/3)))))

(test marshaling-list
  (let ((list (list 1 2 3 (list 4 nil 5) " 6")))
    (is (equalp list
                (load (dump list))))))

(test marshaling-vector
  (let ((v #(1 2 3 #("blub" "this" "that" #\c))))
    (is (equalp v
                (load (dump v)))))
  (let ((a (make-array (list 2 3 4) :initial-element 10)))
    (is (equalp a
                (load (dump a))))))

(test marshaling-hash-table
  (let ((h (alexandria:alist-hash-table `((:a . 1) (:b . 2)))))
    (is (equalp h
                (load (dump h))))
    (let ((h2 (alexandria:alist-hash-table (list (cons :c 3) (cons 'd h) (cons 'e "blub")))))
      (is (equalp h2
                  (load (dump h2))))))
  (let ((h3 (make-hash-table :test #'equal)))
    (is (equalp h3
                (load (dump h3))))))

(test marshaling-object
  (is (eq 'test-object
          (class-name (class-of (load (dump (make-instance 'test-object)))))))
  (is (eq 'test-object
          (class-name (class-of (second (load (dump (list nil (make-instance 'test-object)))))))))
  (let ((a (make-array (list 3 3) :initial-element #\h)))
    (setf (aref a 1 2) (make-instance 'test-object))
    (is (eq 'test-object
            (class-name (class-of (aref (load (dump a)) 1 2)))))))

(test marshaling-with-references
  (let* ((o (make-instance 'test-object))
         (list (load (dump (list o o)))))
    (is (eq (first list)
            (second list))))
  (let* ((h (make-hash-table))
         (l (load (dump (list h h)))))
    (is (eq (first l)
            (second l))))
  (let* ((a (make-array (list 2 2)))
         (l (load (dump (list a a)))))
    (is (eq (first l)
            (second l))))
  (let* ((l (list 1 2 3))
         (ll (load (dump (list l l)))))
    (is (eq (first ll)
            (second ll)))))

(run!)
