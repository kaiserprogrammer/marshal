(defpackage :fmarshal-test
  (:use :cl :fiveam))
(in-package :fmarshal-test)

(def-suite fmarshal)
(in-suite fmarshal)

(defclass test-object ()
  (test-slot))

(defclass test-inherited (test-object)
  (parent-slot))

(test marshaling-simple-objects
  (is (string= "string"
               (fmarshal:load (fmarshal:dump "string"))))
  (is (= 1
         (fmarshal:load (fmarshal:dump 1))))
  (is (= 1.0
         (fmarshal:load (fmarshal:dump 1.0))))
  (is (= 1/3
         (fmarshal:load (fmarshal:dump 1/3)))))

(test marshaling-list
  (let ((list (list 1 2 3 (list 4 nil 5) " 6")))
    (is (equalp list
                (fmarshal:load (fmarshal:dump list))))))

(test marshaling-dotted-lists
  (let ((dlist '((1 . 2) (5 . 3))))
    (is (equalp dlist
                (fmarshal:load (fmarshal:dump dlist))))))

(test marshaling-vector
  (let ((v #(1 2 3 #("blub" "this" "that" #\c))))
    (is (equalp v
                (fmarshal:load (fmarshal:dump v)))))
  (let ((a (make-array (list 2 3 4) :initial-element 10)))
    (is (equalp a
                (fmarshal:load (fmarshal:dump a))))))

(test marshaling-hash-table
  (let ((h (alexandria:alist-hash-table `((:a . 1) (:b . 2)))))
    (is (equalp h
                (fmarshal:load (fmarshal:dump h))))
    (let ((h2 (alexandria:alist-hash-table (list (cons :c 3) (cons 'd h) (cons 'e "blub")))))
      (is (equalp h2
                  (fmarshal:load (fmarshal:dump h2))))))
  (let ((h3 (make-hash-table :test #'equal)))
    (is (equalp h3
                (fmarshal:load (fmarshal:dump h3))))))

(test marshaling-object
  (is (eq 'test-object
          (class-name (class-of (fmarshal:load (fmarshal:dump (make-instance 'test-object)))))))
  (is (eq 'test-object
          (class-name (class-of (second (fmarshal:load (fmarshal:dump (list nil (make-instance 'test-object)))))))))
  (let ((a (make-array (list 3 3) :initial-element #\h)))
    (setf (aref a 1 2) (make-instance 'test-object))
    (is (eq 'test-object
            (class-name (class-of (aref (fmarshal:load (fmarshal:dump a)) 1 2))))))
  (let ((o (make-instance 'test-object)))
    (setf (slot-value o 'test-slot) t)
    (is (eq t
            (slot-value (fmarshal:load (fmarshal:dump o)) 'test-slot))))
  (let ((o (make-instance 'test-inherited)))
    (setf (slot-value o 'test-slot) t)
    (setf (slot-value o 'parent-slot) o)
    (let ((lo (fmarshal:load (fmarshal:dump o))))
      (is (eq t
              (slot-value lo 'test-slot)))
      (is (eq lo
              (slot-value lo 'parent-slot))))))

(test marshaling-with-references
  (let* ((o (make-instance 'test-object))
         (list (fmarshal:load (fmarshal:dump (list o o)))))
    (is (eq (first list)
            (second list))))
  (let* ((h (make-hash-table))
         (l (fmarshal:load (fmarshal:dump (list h h)))))
    (is (eq (first l)
            (second l))))
  (let* ((a (make-array (list 2 2)))
         (l (fmarshal:load (fmarshal:dump (list a a)))))
    (is (eq (first l)
            (second l))))
  (let* ((l (list 1 2 3))
         (ll (fmarshal:load (fmarshal:dump (list l l)))))
    (is (eq (first ll)
            (second ll)))))

(run!)
