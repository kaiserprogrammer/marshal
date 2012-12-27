(defpackage :marshal-test
  (:use :cl :fiveam))
(in-package :marshal-test)

(def-suite marshal)
(in-suite marshal)

(defclass test-object ()
  (test-slot))

(defclass test-inherited (test-object)
  (parent-slot))

(test marshaling-simple-objects
  (is (string= "string"
               (marshal:load (marshal:dump "string"))))
  (is (= 1
         (marshal:load (marshal:dump 1))))
  (is (= 1.0
         (marshal:load (marshal:dump 1.0))))
  (is (= 1/3
         (marshal:load (marshal:dump 1/3)))))

(test marshaling-list
  (let ((list (list 1 2 3 (list 4 nil 5) " 6")))
    (is (equalp list
                (marshal:load (marshal:dump list))))))

(test marshaling-vector
  (let ((v #(1 2 3 #("blub" "this" "that" #\c))))
    (is (equalp v
                (marshal:load (marshal:dump v)))))
  (let ((a (make-array (list 2 3 4) :initial-element 10)))
    (is (equalp a
                (marshal:load (marshal:dump a))))))

(test marshaling-hash-table
  (let ((h (alexandria:alist-hash-table `((:a . 1) (:b . 2)))))
    (is (equalp h
                (marshal:load (marshal:dump h))))
    (let ((h2 (alexandria:alist-hash-table (list (cons :c 3) (cons 'd h) (cons 'e "blub")))))
      (is (equalp h2
                  (marshal:load (marshal:dump h2))))))
  (let ((h3 (make-hash-table :test #'equal)))
    (is (equalp h3
                (marshal:load (marshal:dump h3))))))

(test marshaling-object
  (is (eq 'test-object
          (class-name (class-of (marshal:load (marshal:dump (make-instance 'test-object)))))))
  (is (eq 'test-object
          (class-name (class-of (second (marshal:load (marshal:dump (list nil (make-instance 'test-object)))))))))
  (let ((a (make-array (list 3 3) :initial-element #\h)))
    (setf (aref a 1 2) (make-instance 'test-object))
    (is (eq 'test-object
            (class-name (class-of (aref (marshal:load (marshal:dump a)) 1 2))))))
  (let ((o (make-instance 'test-object)))
    (setf (slot-value o 'test-slot) t)
    (is (eq t
            (slot-value (marshal:load (marshal:dump o)) 'test-slot))))
  (let ((o (make-instance 'test-inherited)))
    (setf (slot-value o 'test-slot) t)
    (setf (slot-value o 'parent-slot) o)
    (let ((lo (marshal:load (marshal:dump o))))
      (is (eq t
              (slot-value lo 'test-slot)))
      (is (eq lo
              (slot-value lo 'parent-slot))))))

(test marshaling-with-references
  (let* ((o (make-instance 'test-object))
         (list (marshal:load (marshal:dump (list o o)))))
    (is (eq (first list)
            (second list))))
  (let* ((h (make-hash-table))
         (l (marshal:load (marshal:dump (list h h)))))
    (is (eq (first l)
            (second l))))
  (let* ((a (make-array (list 2 2)))
         (l (marshal:load (marshal:dump (list a a)))))
    (is (eq (first l)
            (second l))))
  (let* ((l (list 1 2 3))
         (ll (marshal:load (marshal:dump (list l l)))))
    (is (eq (first ll)
            (second ll)))))

(run!)
