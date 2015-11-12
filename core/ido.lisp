;;;; IMAP4 for Common Lisp
;;;; IMAP Data Objects
;;
;; IMAP data objects (IDO) are the program representation of read IMAP
;; protocol data.  They have associated with them the reader function.
;; It's up to the connection to map protocol text with the lisp object.
(in-package :org.drurowin.net.imap4.1)

(defgeneric data-object-reader (imap-data-object)
  (:documentation "The protocol reader function for the IMAP data object."))

(defgeneric (setf data-object-reader) (reader imap-data-object)
  (:documentation "Set the protocol reader function for the IMAP data object.

Use the reader function NIL to remove the function."))

(defgeneric find-ido (designator)
  (:documentation "Return the IMAP data object for the designator."))

(defclass ido-class (standard-class)
  ((reader :initarg :reader))
  (:documentation "metaclass of IMAP4 data objects"))

(defmethod closer-mop:validate-superclass ((o ido-class) (class standard-class)) t)

(defun check-ido-reader (reader)
  (check-type reader list)
  (let ((thunk (car reader)))
    (check-type thunk (or null symbol function))))

(defmethod initialize-instance :before ((o ido-class) &key reader &allow-other-keys)
  (check-ido-reader reader))
(defmethod initialize-instance :after ((o ido-class) &key reader &allow-other-keys)
  (setf (slot-value o 'reader) (car reader)))

(defmethod reinitialize-instance :before ((o ido-class) &key reader &allow-other-keys)
  (check-ido-reader reader))
(defmethod reinitialize-instance :after ((o ido-class) &key (reader nil re-reader) &allow-other-keys)
  (when re-reader (setf (slot-value o 'reader) (car reader))))

(defmethod data-object-reader ((ido ido-class))
  (when (slot-boundp ido 'reader)
    (slot-value ido 'reader)))

(defmethod (setf data-object-reader) ((val function) (ido ido-class))
  (setf (slot-value ido 'reader) val))

(defmethod (setf data-object-reader) ((val symbol) (ido ido-class))
  (setf (slot-value ido 'reader) val))

(defmethod (setf data-object-reader) ((val null) (ido ido-class))
  (slot-makunbound ido 'reader))

(defclass ido-object () ()
  (:documentation "parent class of IMAP4 data objects"))

(defmethod data-object-reader ((ido ido-object))
  (data-object-reader (class-of ido)))

(defmethod (setf data-object-reader) (val (ido ido-object))
  (setf (data-object-reader (class-of ido)) val))

(defmethod find-ido ((o symbol))
  (find-class (if (or (keywordp o)
                      (eql (symbol-package o) *package*))
                  (find-symbol (symbol-name o) :imap4-protocol)
                  o)))

(defmethod find-ido ((o ido-object)) o)

(indentation define-imap-data-object (as defclass))
(defmacro define-imap-data-object (name direct-superclasses slots &rest options)
  (let ((metaclass (or (cadr (assoc :metaclass options)) 'ido-class)))
    `(defclass ,(if (keywordp name)
                    (intern (symbol-name name) :imap4-protocol)
                    name)
         ,(remove-duplicates (cons 'ido-object direct-superclasses))
       ,slots
       (:metaclass ,metaclass)
       ,@(remove :metaclass options :key #'car))))
