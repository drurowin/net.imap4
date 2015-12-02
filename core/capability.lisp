;;;; IMAP4 for Common Lisp
;;;; mail server capabilities
(in-package :org.drurowin.net.imap4.core.1)

(defgeneric capability-inherits-from (capability)
  (:documentation "A list of capabilities the capability inherits from."))

(defgeneric find-applicable-ido (capability name)
  (:documentation "Search the capability tree to find the IDO for the protocol NAME."))

(defgeneric find-capability (designator)
  (:documentation "The `capability' from DESIGNATOR."))

(defclass capability ()
  ((name :initarg :name)
   (inherits-from)
   (documentation :initarg :documentation)
   (ido-map :initform (make-hash-table :test #'equal))))

(defmethod initialize-instance :before ((o capability) &key name documentation direct-ido inherits-from &allow-other-keys)
  (check-type name string)
  (check-type documentation (or null string))
  (check-type direct-ido (or symbol ido-class list))
  (check-type inherits-from (or null symbol capability list)))

(defmethod initialize-instance :after ((o capability) &key direct-ido inherits-from documentation &allow-other-keys)
  (collect (inherit)
    (when inherits-from
      (typecase inherits-from
        (capability (inherit inherits-from))
        (symbol (inherit (find-class inherits-from)))
        (list (dolist (capability inherits-from)
                (etypecase capability
                  (null nil)
                  (symbol (inherit (find-class capability)))
                  (capability (inherit capability)))))))
    (setf (slot-value o 'inherits-from) (inherit)))
  (collect (ido)
    (typecase direct-ido
      (symbol (let ((ido (find-ido direct-ido)))
                (when ido (ido (cons (symbol-name (class-name ido)) ido)))))
      (ido-class (ido (cons (symbol-name (class-name direct-ido)) direct-ido)))
      (list (dolist (ido direct-ido)
              (let ((ido (find-ido ido)))
                (when ido (ido (cons (symbol-name (class-name ido)) ido)))))))
    (dolist (pair (ido))
      (setf (gethash (car pair) (slot-value o 'ido-map)) (cdr pair))))
  (setf (slot-value o 'documentation) documentation))
