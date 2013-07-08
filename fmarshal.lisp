(defpackage :fmarshal
  (:use :cl)
  (:shadow :load)
  (:export :load :dump))

(in-package :fmarshal)

(defmacro maybe-dump-ref (o &body body)
  (let ((g (gensym)))
    `(let ((,g ,o))
       (if (gethash ,g *refs*)
           (dump-ref (gethash ,g *refs*) stream)
           (progn (setf (gethash ,g *refs*) (next-ref))
                  ,@body)))))

(defvar *refs*)
(defvar *next-ref*)

(defun dump (object &optional stream)
  (let ((*refs* (make-hash-table))
        (*next-ref* 0))
    (if stream
        (dump-helper object stream)
        (with-output-to-string (stream)
          (dump-helper object stream)))))

(defgeneric dump-helper (object stream))
(defmethod dump-helper ((s string) stream)
  (format stream "~w" s))
(defmethod dump-helper ((c character) stream)
  (format stream "~w" c))
(defmethod dump-helper ((n number) stream)
  (format stream "~w" n))
(defmethod dump-helper ((l cons) stream)
  (maybe-dump-ref l
    (progn
      (format stream "(fmarshal::dumped :list ~a (" (gethash l *refs*))
      (do ((e l (cdr e)))
          ((or (atom e) (null e)))
        (write-string " " stream)
        (if (and (atom (cdr e))
                 (not (null (cdr e))))
            (progn (dump-helper (car e) stream)
                   (write-string " . " stream)
                   (dump-helper (cdr e) stream))
            (dump-helper (car e) stream)))
      (write-string ")" stream)
      (write-string ")" stream))))

(defmethod dump-helper ((a array) stream)
  (maybe-dump-ref a
    (progn
      (format stream "(fmarshal::dumped :array ~a ~a " (gethash a *refs*) (array-dimensions a))
      (dump-array a stream)
      (write-string ")" stream))))

(defmethod dump-helper ((h hash-table) stream)
  (maybe-dump-ref h
    (progn
      (write-string "(fmarshal::dumped :hash-table " stream)
      (format stream "~a ~a " (gethash h *refs*) (hash-table-test h))
      (dump-hash-table h stream)
      (write-string ")" stream))))

(defmethod dump-helper ((s symbol) stream)
  (format stream "~s" s))

(defun next-ref ()
  (incf *next-ref*))

(defmethod dump-helper (object stream)
  (maybe-dump-ref object
    (progn
      (write-string "(fmarshal::dumped " stream)
      (format stream "~a " (gethash object *refs*))
      (format stream "~s " (class-name (class-of object)))
      (dump-slot-definitions object stream)
      (write-string ")" stream))))

(defun dump-ref (ref stream)
  (format stream "(fmarshal::dumped :ref ~a)" ref))

(defun dump-array (a stream)
  (dump-array-helper (array-dimensions a) a nil stream))

(defun dump-array-helper (dim a ref stream)
  (if (null dim)
      (dump-helper (apply #'aref a (reverse ref)) stream)
      (progn
        (write-string "(" stream)
        (dotimes (i (car dim))
          (write-string " " stream)
          (dump-array-helper (cdr dim) a (cons i ref) stream))
        (write-string ")" stream))))

(defun load-array (ref dim list)
  (let ((a (make-array dim)))
    (setf (gethash ref *refs*) a)
    (load-array-helper list a (list) dim)
    a))

(defun load-array-helper (list a ref dim)
  (if (null dim)
      (setf (apply #'aref a (reverse ref)) list)
      (loop for i from 0 below (car dim)
         for e in list
         do (load-array-helper (load-helper e) a (cons i ref) (cdr dim)))))

(defun dump-hash-table (h stream)
  (write-string "(" stream)
  (loop for v being the hash-values in h using (hash-key k)
     do (progn
          (write-string "(" stream)
          (dump-helper k stream)
          (write-string " . " stream)
          (dump-helper v stream)
          (write-string ")" stream)))
  (write-string ")" stream))

(defun load (stream)
  (when stream
    (let ((*refs* (make-hash-table)))
      (unmarshalize stream))))

(defun unmarshalize (stream)
  (load-helper
   (if (stringp stream)
       (read-from-string stream)
       (read stream))))


(defun load-helper (desc)
  (if (or (not (consp desc)) (not (eq 'fmarshal::dumped (first desc))))
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
  (do ((e list (cdr e)))
      ((or (atom e) (null e)))
    (if (atom (cdr e))
        (progn (setf (car e) (load-helper (car e)))
               (setf (cdr e) (load-helper (cdr e))))
        (setf (car e) (load-helper (car e)))))
  (setf (gethash ref *refs*) list)
  list)

(defun load-object (ref class slots-desc)
  (let ((o (make-instance class)))
    (setf (gethash ref *refs*) o)
    (loop for (slot-name . slot-value) in slots-desc
       do (setf (slot-value o slot-name) (load-helper slot-value)))
    o))

(defun load-hash-table (ref test desc)
  (let ((h (make-hash-table :test test)))
    (setf (gethash ref *refs*) h)
    (loop for (k . v) in desc
       do (setf (gethash (load-helper k) h) (load-helper v)))
    h))

(defun dump-slot-definitions (object stream)
  (write-string "(" stream)
  (let ((slots (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of object)))))
    (loop for slot in slots
       when (slot-boundp object slot)
       do (progn
            (format stream "(~s . " slot)
            (dump-helper (slot-value object slot) stream)
            (write-string ")" stream))))
  (write-string ")" stream))
