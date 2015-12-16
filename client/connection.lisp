;;;; IMAP4 for Common Lisp
;;;; client connections
(in-package :org.drurowin.net.imap4.client.1)

(defmacro with-open-imap4-client ((var type &rest options) &body body)
  `(generic-open:with-open-generic-stream (,var (make-instance ,type ,@options)) ,@body))

(defclass fundamental-imap4-client (core:imap4-connection)
  ((tag :initform 0)
   (mailbox)
   (connect-response))
  (:documentation "Parent class of IMAP client connections.")
  (:default-initargs
   :id-test #'equalp))

(defgeneric connection-state (connection)
  (:method ((c fundamental-imap4-client))
    (if (open-stream-p (core:imap4-connection-stream c))
        (if (slot-boundp c 'connect-response)
            (if (slot-boundp c 'mailbox) :selected
                (typecase (slot-value c 'connect-response)
                  (null :authenticated)
                  (imap4-protocol:ok :not-authenticated)
                  (imap4-protocol:preauth :authenticated)
                  (imap4-protocol:bye :closed)))
            :not-authenticated)
        :closed)))

(defmethod imap4-client-tag ((o fundamental-imap4-client))
  (format nil "a~D" (incf (slot-value o 'tag))))

(defclass inet-imap4-client (fundamental-imap4-client)
  ((socket)
   (host :initarg :host :reader imap4-client-host)
   (port :initarg :port :reader imap4-client-port)
   (sslp :initarg :sslp :reader imap4-client-sslp))
  (:documentation "Client connection to a server on the internet."))

(defmethod print-object ((o inet-imap4-client) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~A:~D~@[ with SSL~]"
            (slot-value o 'host) (slot-value o 'port) (slot-value o 'sslp))))

(defmethod generic-open:open-stream ((s inet-imap4-client) &key &allow-other-keys)
  (unless (open-stream-p s)
    (let ((socket (usocket:socket-connect (slot-value s 'host) (slot-value s 'port) :element-type '(unsigned-byte 8))))
      (setf (slot-value s 'socket) socket
            (core:imap4-connection-stream s) (if (slot-value s 'sslp)
                                            (cl+ssl:make-ssl-client-stream (usocket:socket-stream socket))
                                            (usocket:socket-stream socket)))))
  s)

(defmethod close ((s inet-imap4-client) &key &allow-other-keys)
  (usocket:socket-close (slot-value s 'socket)))

(defmethod mp:parse-response ((conn fundamental-imap4-client))
  (let ((in (slot-value conn 'stream)))
    (let* ((tag (core:read-imap4 in))
           (ido (core:read-imap4 in))
           (data nil nil))
      (let ((maybe (ignore-errors (parse-integer ido))))
        (when maybe
          (setf ido (core:read-imap4 in)
                data maybe)))
      (let ((ido-object (core:find-applicable-ido (make-instance 'core:capability
                                                    :inherits-from (core:imap4-connection-capabilities conn))
                                                  ido)))
        (funcall (core:data-object-reader ido-object) ido-object in tag data)))))

(defmethod mp:handle-response :after ((conn fundamental-imap4-client) (resp status-response))
  (when (slot-value resp 'tag)
    (mp:finish-message-processing conn (slot-value resp 'tag))))
