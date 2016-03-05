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

(defmethod reinitialize-instance :after ((o fundamental-imap4-client) &key &allow-other-keys)
  (setf (slot-value o 'tag) 0)
  (slot-makunbound o 'mailbox)
  (slot-makunbound o 'connect-response))

(defmethod mp:start-processing-messages :after ((conn fundamental-imap4-client))
  (setf (slot-value conn 'connect-response) (mp:parse-response conn)))

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

(defmethod reinitialize-instance :after ((o inet-imap4-client)
                                         &key (host nil hostp) (port nil portp) (sslp nil sslpp) &allow-other-keys)
  (when (slot-boundp o 'socket)
    (usocket:socket-close (slot-value o 'socket))
    (slot-makunbound o 'socket))
  (when hostp (setf (slot-value o 'host) host))
  (when portp (setf (slot-value o 'port) port))
  (when sslpp (setf (slot-value o 'sslp) sslp)))

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
  (let ((in (core:imap4-connection-stream conn)))
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

(defmethod mp:no-applicable-handler ((conn fundamental-imap4-client) resp)
  (warn "No handler specified for ~A response.  Dropping response."
        (type-of resp)))

(defmethod mp:no-applicable-handler ((conn fundamental-imap4-client) (resp imap4-protocol:capability))
  "Ignore unexpected CAPABILITY response.")

(defmethod mp:no-applicable-handler ((conn fundamental-imap4-client) (resp status-response))
  "Do nothing: handled mainly by `mp:handle-response'.")

(defmethod mp:handle-response :after ((conn fundamental-imap4-client) (resp status-response))
  (when (slot-value resp 'tag)
    (mp:finish-message-processing conn (slot-value resp 'tag))))

;;;;========================================================
;;;; commands
(defgeneric read-password (method &key user domain)
  (:documentation "Read a password.")
  #+darwin
  (:method ((o (eql :keychain)) &key user domain)
    "Read the password from the user's password store."
    (when (find-package :org.drurowin.security)
      (funcall (find-symbol "PASSWORD" :org.drurowin.security)
               user domain)))
  (:method ((o (eql :emacs)) &key user domain)
    "Read the password using Emacs, when connected via SWANK.

If the Emacs variable `slime-enable-evaluate-in-emacs' is not nil, the
call will trap in Emacs."
    (when (find-package :swank)
      (funcall (find-symbol "EVAL-IN-EMACS" :swank)
               (list 'read-passwd (format nil "Password for ~:[<#unknown user>~;~:*~A~]@~:[<#unknown domain>~;~:*~A~]: "
                                          user domain))))))

(defmethod mp:send-datum ((o fundamental-imap4-client) (val (eql :password)) _ &key method user)
  "Read a password using :METHOD.  Use :USER to notify which user password is for.

See the generic function `read-password' for acceptable values to :METHOD.")
