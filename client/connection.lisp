(cl:in-package :org.drurowin.net.imap4.1)

(defmacro with-open-imap4-client ((var type &rest options) &body body)
  `(with-open-generic-stream (,var (make-instance ,type ,@options)) ,@body))

(defclass fundamental-imap4-client (fundamental-imap4-connection)
  ((tag :initform 0)
   (responses :initform (make-hash-table :test #'equal)))
  (:documentation "Parent class of IMAP client connections."))

(defmethod initialize-instance :after ((o fundamental-imap4-client) &key &allow-other-keys)
  (with-slots (responses) o
    (setf (gethash "CAPABILITY" responses) (find-class 'capability)
          (gethash "OK" responses) (find-class 'ok-response)
          (gethash "NO" responses) (find-class 'no-response)
          (gethash "BAD" responses) (find-class 'bad-response)
          (gethash "PREAUTH" responses) (find-class 'preauth-response)
          (gethash "BYE" responses) (find-class 'bye-response))))

(defmethod response-processor ((c fundamental-imap4-client) (r string))
  (gethash r (slot-value c 'responses)))

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

(defmethod open-stream ((s inet-imap4-client) &key &allow-other-keys)
  (unless (open-stream-p s)
    (let ((socket (usocket:socket-connect (slot-value s 'host) (slot-value s 'port) :element-type '(unsigned-byte 8))))
      (setf (slot-value s 'socket) socket
            (imap4-connection-stream s) (if (slot-value s 'sslp)
                                            (cl+ssl:make-ssl-client-stream (usocket:socket-stream socket))
                                            (usocket:socket-stream socket)))))
  s)

(defmethod close-stream ((s inet-imap4-client) &key &allow-other-keys)
  (usocket:socket-close (slot-value s 'socket)))
