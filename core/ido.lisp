(cl:in-package :org.drurowin.net.imap4.1)

(defclass ido-class (standard-class)
  ((imap-name :documentation "The default IMAP name of the object.")
   (processor :documentation "The response processor."))
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

(defmethod shared-initialize :after ((o ido-class) slot-names &rest args &key name &allow-other-keys)
  (declare (ignore slot-names))
  (symbol-macrolet ((imap-name (car (getf args :imap-name)))
                    (processor (car (getf args :processor))))
    (when imap-name
      (format *debug-io* "IDO: ~S IMAP Name: ~S Processor: ~S~&" name imap-name processor))
    (if imap-name (setf (slot-value o 'imap-name) imap-name)
        (slot-makunbound o 'imap-name))
    (if processor (setf (slot-value o 'processor) (eval processor))
        (slot-makunbound o 'processor))))

(defclass fundamental-imap4-data-object () ()
  (:metaclass ido-class))

(defmethod data-object-processor ((o fundamental-imap4-data-object)) (data-object-processor (class-of o)))

(defmethod data-object-processor ((o symbol)) (data-object-processor (find-class o)))

(defmethod (setf data-object-processor) (value (data-object fundamental-imap4-data-object))
  (setf (data-object-processor (class-of data-object)) value))

(defmethod data-object-imap-name ((o fundamental-imap4-data-object) s) (data-object-imap-name (class-of o) s))

(defvar %imap-data-objects% (make-hash-table :test #'equal))

(defun ensure-ido (ido)
  (handler-case
      (setf (gethash (data-object-imap-name ido nil) %imap-data-objects%) ido)
    (unbound-slot ())))

(defgeneric find-ido (ido)
  (:method ((o string)) (gethash o %imap-data-objects%))
  (:method (o) (find-ido (data-object-imap-name o nil))))

(defmacro define-imap4-data-object (name direct-superclasses direct-slots &rest options)
  `(progn
     (defclass ,name (fundamental-imap4-data-object ,@direct-superclasses)
       ,(mapcar (lambda (slot)
                  (if (consp slot) slot
                      `(,slot :initarg ,(intern (symbol-name slot) :keyword)
                              :initform nil
                              :reader ,(intern (format nil "~A-~A" name slot)))))
         direct-slots)
       (:metaclass ido-class)
       (:imap-name ,(or (cadr (assoc :imap-name options)) (symbol-name name)))
       (:processor ,(cadr (assoc :processor options)))
       ,@(remove-if (lambda (o)
                      (find o '(:imap-name :processor)))
                    options
                    :key #'car))
     (ensure-ido (find-class ',name))
     ',name))

(define-imap4-data-object continuation-request ()
  (text)
  (:documentation "Sent by the server to notify the client to continue sending data.")
  (:imap-name "+"))

(defclass status-response ()
  ((tag :initarg :tag :initform nil :reader status-response-tag)
   (response-code :initarg :response-code :initform nil :reader status-response-code)
   (text :initarg :text :initform nil :reader status-response-text)))

(define-imap4-data-object ok-response (status-response) ()
  (:documentation "7.1.1 Server informational message.")
  (:imap-name "OK"))

(define-imap4-data-object no-response (status-response) ()
  (:documentation "7.1.2 Operation error message.")
  (:imap-name "NO"))

(define-imap4-data-object bad-response (status-response) ()
  (:documentation "7.1.3 Protocol and general purpose error message.")
  (:imap-name "BAD"))

(define-imap4-data-object preauth-response (status-response) ()
  (:documentation "7.1.4 Connection is started in authenticated state.")
  (:imap-name "PREAUTH"))

(define-imap4-data-object bye-response (status-response) ()
  (:documentation "7.1.5 Server is closing the connection.

When used as the greeting the server is not accepting connections from
the client.")
  (:imap-name "BYE"))

(define-imap4-data-object capability ()
  (imaprev extras)
  (:documentation "Capabilities provided by the server."))

(define-imap4-data-object lsub ()
  (name delimiter attributes))

(define-imap4-data-object list-response ()
  (name delimiter atributes)
  (:imap-name "LIST"))
