;;;; IMAP4 for Common Lisp
;;;; mail server capabilities
(in-package :org.drurowin.net.imap4.core.1)

(defgeneric capability-inherits-from (capability)
  (:documentation "A list of capabilities the capability inherits from.

Capabilities at the start of the list inherit from capabilities towards
the end of the list (similar to the class precendence list)."))

(defgeneric find-applicable-ido (capability name)
  (:documentation "Search the capability tree to find the IDO for the protocol NAME."))

(defgeneric (setf find-applicable-ido) (ido capability name)
  (:documentation "Cause the capability to directly implement the IDO.

If the IDO is NIL, the IDO is removed as a direct implementation."))

(defgeneric no-applicable-ido (capability name)
  (:documentation "The default action to take when no IDO can be gotten from NAME."))

(defgeneric find-capability (designator)
  (:documentation "The `capability' from DESIGNATOR."))

(defvar %capabilities% (make-hash-table :test #'equalp))

(defmacro define-capability (name inherits-from &optional imap-name documentation)
  (check-type name (and symbol (not null)))
  (check-type imap-name (or string null))
  `(progn
     (defparameter ,name
       (make-instance 'capability :name ,(or imap-name (symbol-name name))
         :documentation ,documentation
         :inherits-from (list ,@inherits-from)))
     (eval-when (:load-toplevel :execute)
       (setf (gethash ,(or imap-name (symbol-name name)) %capabilities%) ,name))
     ',name))

(defclass capability ()
  ((name :initarg :name)
   (inherits-from)
   (documentation :initarg :documentation)
   (ido-map :initform (make-hash-table :test #'equal))))

(defmethod find-capability ((o capability)) o)
(defmethod find-capability ((o symbol)) (find-capability (symbol-value o)))
(defmethod find-capability ((o string)) (gethash o %capabilities%))

(indentation define-imap-data-object (as defclass))
(defmacro define-imap-data-object (name direct-superclasses slots &rest options)
  "Define NAME as an IMAP data object class.

The :READER option is specific to DEFINE-IMAP-DATA-OBJECT.  Its argument
is a function designator evaluated in the context of the D-I-D-O.  The
function should be of four arguments: the response class, the stream to
read data from, the response tag, and a numeric datum or NIL.

  `lambda/imap4' may be used to generate reader functions that abstract
  away the protocol.

The option :CAPABILITY must be present to set the IDO to a capability."
  (let ((metaclass (or (cadr (assoc :metaclass options)) 'ido-class))
        (reader (cadr (assoc :reader options)))
        (capability (or (cadr (assoc :capability options))
                        (error "Option :CAPABILITY not specified.")))
        (name (if (keywordp name)
                  (intern (symbol-name name) :imap4-protocol)
                  name))
        (options (copy-list options)))
    (setf options (remove :metaclass options :key #'car)
          options (remove :reader options :key #'car)
          options (remove :capability options :key #'car))
    `(progn
       (defclass ,name
           ,(remove-duplicates (append direct-superclasses (list 'ido-object)))
         ,slots
         (:metaclass ,metaclass)
         ,@options)
       (eval-when (:load-toplevel :execute)
         (setf (data-object-reader (find-class ',name)) ,reader
               (find-applicable-ido ,capability ,(symbol-name name)) (find-class ',name)))
       ',name)))

(defmethod print-object ((o capability) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~S" (slot-value o 'name))))

(defmethod capability-inherits-from ((o capability))
  (copy-list (slot-value o 'inherits-from)))

(defmethod initialize-instance :before ((o capability) &key name documentation direct-ido inherits-from &allow-other-keys)
  (check-type name (or null string))
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
    (when direct-ido
      (typecase direct-ido
        (symbol (let ((ido (find-ido direct-ido)))
                  (when ido (ido (cons (symbol-name (class-name ido)) ido)))))
        (ido-class (ido (cons (symbol-name (class-name direct-ido)) direct-ido)))
        (list (dolist (ido direct-ido)
                (let ((ido (find-ido ido)))
                  (when ido (ido (cons (symbol-name (class-name ido)) ido))))))))
    (dolist (pair (ido))
      (setf (gethash (car pair) (slot-value o 'ido-map)) (cdr pair))))
  (setf (slot-value o 'documentation) documentation))

(defun toposort-capabilities (capability)
  (check-type capability capability)
  (do* ((S (do* ((acc ())
                 (breadcrumb (make-hash-table))
                 (capabilities (cons capability nil)
                               (append (capability-inherits-from capability)
                                       (cdr capabilities)))
                 (capability (car capabilities) (car capabilities)))
                ((null capabilities) acc)
             (unless (gethash capability breadcrumb)
               (push capability acc))
             (setf (gethash capability breadcrumb) capability)))
        (R (collect (pair)
             (dolist (c S)
               (do ((ihf (list* c (capability-inherits-from c))
                         (cdr ihf)))
                   ((= (length ihf) 1))
                 (pair (cons (car ihf) (cadr ihf)))))
             (remove-duplicates (pair) :test #'equalp)))
        (order nil))
       ((null S) (reverse order))
    (let ((next (block next
                  (dolist (c S (return-from toposort-capabilities
                                 (values nil
                                         (length order)
                                         (length (set-difference S order)))))
                    (block test
                      (dolist (opair R (return-from next c))
                        (when (eql c (cdr opair))
                          (return-from test))))))))
      (push next order)
      (setf S (remove next S)
            R (do ((acc ())
                   (pair (car R) (car rest))
                   (rest (cdr R) (cdr rest)))
                  ((null pair) acc)
                (unless (or (eql next (car pair))
                            (eql next (cdr pair)))
                  (push pair acc)))))))

(defmethod find-applicable-ido ((o capability) (ido string))
  (dolist (capability (toposort-capabilities o))
    (let ((maybe (gethash ido (slot-value capability 'ido-map))))
      (when maybe (return-from find-applicable-ido maybe))))
  (no-applicable-ido o ido))

(defmethod (setf find-applicable-ido) ((ido ido-class) (c capability) (name string))
  (setf (gethash name (slot-value c 'ido-map)) ido))

(defmethod (setf find-applicable-ido) ((ido null) (c capability) (name string))
  (remhash name (slot-value c 'ido-map)))

(define-condition no-applicable-ido (error)
  ((capability :initarg :capability :initform nil :reader no-applicable-ido-capability)
   (ido :initarg :ido :initform nil :reader no-applicable-ido-ido))
  (:report (lambda (c s)
             (format s "The IDO ~S cannot be resolved in ~S."
                     (no-applicable-ido-ido c)
                     (no-applicable-ido-capability c)))))

(defmethod no-applicable-ido (c ido)
  (error 'no-applicable-ido :capability c :ido ido))
