(cl:in-package :org.drurowin.net.imap4.1)

(defclass ido-class (standard-class)
  ((imap-name :initarg :imap-name
              :documentation "The default IMAP name of the object.")
   (processor :initarg :processor
              :documentation "The response processor."))
  (:documentation "Metaclass of IMAP4 data objects."))

(defmethod closer-mop:validate-superclass ((o ido-class) (class standard-class)) t)

(defmethod data-object-imap-name ((o ido-class) (s null)) (slot-value o 'imap-name))

(defmethod data-object-imap-name ((o symbol) s) (data-object-imap-name (find-class o) s))

(defmethod data-object-processor ((o ido-class)) (slot-value o 'processor))

(defmethod (setf data-object-processor) (fn (o ido-class))
  (check-type fn function)
  (setf (slot-value o 'processor) fn))

(defmethod (setf data-object-processor) (fn (o symbol))
  (setf (data-object-processor (find-class o)) fn))

(defmethod initialize-instance :around ((o ido-class) &rest args &key name &allow-other-keys)
  (symbol-macrolet ((imap-name (getf args :imap-name)))
    (setf imap-name (or (car imap-name) (symbol-name name)))
    (check-type imap-name string)
    (apply #'call-next-method o args))
  o)

(defclass fundamental-imap4-data-object () ()
  (:metaclass ido-class))

(defmethod data-object-processor ((o fundamental-imap4-data-object)) (data-object-processor (class-of o)))

(defmethod data-object-processor ((o symbol)) (data-object-processor (find-class o)))

(defmethod (setf data-object-processor) (value (data-object fundamental-imap4-data-object))
  (setf (data-object-processor (class-of data-object)) value))

(defmethod data-object-imap-name ((o fundamental-imap4-data-object) s) (data-object-imap-name (class-of o) s))

(defmacro define-imap4-data-object (name direct-superclasses direct-slots &rest options)
  (let* ((lisp-name (if (consp name) (car name) name))
         (imap-name (if (consp name) (getf (cdr name) :imap-name) (symbol-name lisp-name)))
         (processor (cdr (assoc :processor options))))
    `(defclass ,name (fundamental-imap4-data-object ,@direct-superclasses)
       ,(mapcar (lambda (slot)
                  (if (consp slot) slot
                      `(,slot :initarg ,(intern (symbol-name slot) :keyword)
                              :initform nil
                              :reader ,(intern (format nil "~A-~A" lisp-name slot)))))
         direct-slots)
       (:metaclass ido-class)
       (:imap-name ,imap-name)
       (:processor ,processor)
       ,@options)))

(define-imap4-data-object continuation-request ()
  (text)
  (:documentation "Sent by the server to notify the client to continue sending data."))

(define-imap4-data-object capability ()
  (imaprev extras)
  (:documentation "Capabilities provided by the server."))
