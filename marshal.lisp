(defpackage :marshal
  (:use :cl)
  (:shadow :load)
  (:export :load :dump))

(in-package :marshal)

(defmacro maybe-dump-ref (o &body body)
  (let ((g (gensym)))
    `(let ((,g ,o))
       (if (gethash ,g *refs*)
           (dump-ref (gethash ,g *refs*))
           (progn (setf (gethash ,g *refs*) (next-ref))
                  ,@body)))))

(defvar *refs*)
(defvar *next-ref*)

(defun dump (object)
  (let ((*refs* (make-hash-table))
        (*next-ref* 0))
    (dump-helper object)))

(defgeneric dump-helper (object))
(defmethod dump-helper ((s string))
  (format nil "~w" s))
(defmethod dump-helper ((c character))
  (format nil "~w" c))
(defmethod dump-helper ((n number))
  (format nil "~w" n))
(defmethod dump-helper ((l cons))
  (maybe-dump-ref l
   (format nil "(marshal::dumped :list ~a ~w)" (gethash l *refs*) (mapcar #'dump-helper l))))
(defmethod dump-helper ((a array))
  (maybe-dump-ref a
    (format nil "(marshal::dumped :array ~a ~w ~w)" (gethash a *refs*) (array-dimensions a) (dump-array a))))
(defmethod dump-helper ((h hash-table))
  (maybe-dump-ref h
    (format nil "(marshal::dumped :hash-table ~a ~w ~w)" (gethash h *refs*) (hash-table-test h) (dump-hash-table h))))

(defmethod dump-helper ((s symbol))
  (format nil "~w" s))

(defun next-ref ()
  (incf *next-ref*))

(defmethod dump-helper (object)
  (maybe-dump-ref object
    (format nil "(marshal::dumped ~a ~w ~w)" (gethash object *refs*) (class-name (class-of object)) (dump-slot-definitions object))))

(defun dump-ref (ref)
  (format nil "(marshal::dumped :ref ~a)" ref))

(defun dump-array (a)
  (dump-array-helper (array-dimensions a) a nil))

(defun dump-array-helper (dim a ref)
  (if (null dim)
      (dump-helper (apply #'aref a (reverse ref)))
      (let ((list (list)))
        (dotimes (i (car dim))
          (push (dump-array-helper (cdr dim) a (cons i ref)) list))
        (nreverse list))))

(defun load-array (ref dim list)
  (let ((a (make-array dim)))
    (load-array-helper list a (list))
    (setf (gethash ref *refs*) a)
    a))

(defun load-array-helper (list a ref)
  (if (atom list)
      (setf (apply #'aref a (reverse ref)) (load list))
      (loop for i from 0
         for e in list
         do (load-array-helper e a (cons i ref)))))

(defun dump-hash-table (h)
  (loop for v being the hash-values in h using (hash-key k)
     collect (cons (dump-helper k)
                   (dump-helper v))))

(defun load (stream)
  (let ((*refs* (make-hash-table)))
    (load-helper stream)))

(defun load-helper (stream)
    (marshalize
   (if (stringp stream)
       (read-from-string stream)
       (read stream))))


(defun marshalize (desc)
  (if (or (not (consp desc)) (not (eq 'marshal::dumped (first desc))))
      (if (consp desc)
          (mapcar #'load-helper desc)
          desc)
      (if (eq :list (second desc))
          (load-list (third desc) (fourth desc))
          (if (eq :hash-table (second desc))
              (load-hash-table (third desc) (fourth desc) (fifth desc))
              (if (eq :array (second desc))
                  (load-array (third desc) (fourth desc) (fifth desc))
                  (if (eq :ref (second desc))
                      (gethash (third desc) *refs*)
                      (load-object (second desc) (third desc) (fourth desc))))))))

(defun load-list (ref list)
  (let ((l (mapcar #'load-helper list)))
    (setf (gethash ref *refs*) l)
    l))

(defun load-object (ref class slots-desc)
  (let ((o (make-instance class)))
    (loop for (slot-name . slot-value) in slots-desc
       do (setf (slot-value o (load-helper slot-name)) (load-helper slot-value)))
    (setf (gethash ref *refs*) o)
    o))

(defun load-hash-table (ref test desc)
  (let ((h (make-hash-table :test test)))
    (loop for (k . v) in desc
       do (setf (gethash (load-helper k) h) (load-helper v)))
    (setf (gethash ref *refs*) h)
    h))

(defun dump-slot-definitions (object)
  (let ((slots (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of object)))))
    (loop for slot in slots
       when (slot-boundp object slot)
       collect (cons slot (slot-value object slot)))))
