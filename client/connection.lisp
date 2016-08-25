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

(defmethod core:imap4-connection-capabilities ((o fundamental-imap4-client))
  (if (slot-boundp o 'core::capabilities)
      (call-next-method)
      (list +imap4+)))

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
  (:documentation "Client connection to a server on the internet.")
  (:default-initargs :sslp nil))

(defmethod shared-initialize :around
    ((o inet-imap4-client) slots &rest args &key port sslp &allow-other-keys)
  (when (or (eql slots t) (find 'port slots))
    (check-type port (or null (integer 0))))
  (when (not port)
    (setf (getf args :port) (if sslp 993 143)))
  (apply #'call-next-method o slots args))

(defmethod reinitialize-instance :after ((o inet-imap4-client) &key &allow-other-keys)
  (when (slot-boundp o 'socket)
    (usocket:socket-close (slot-value o 'socket))
    (slot-makunbound o 'socket)))

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
  "Update the capabilities list."
  (let ((new (remove nil (mapcar #'core:find-capability (slot-value resp 'list))))
        (old (if (slot-boundp conn 'core::capabilities) (slot-value conn 'core::capabilities) (list))))
    (format *debug-io* "New: ~S~%Old: ~S~%" new old)
    (setf (slot-value conn 'core::capabilities) new)))

(defmethod mp:no-applicable-handler ((conn fundamental-imap4-client) (resp status-response))
  "Do nothing: handled mainly by `mp:handle-response'.")

(defmethod mp:handle-response :after ((conn fundamental-imap4-client) (resp status-response))
  (when (slot-value resp 'tag)
    (mp:finish-message-processing conn (slot-value resp 'tag))))

;;;;========================================================
;;;; commands

;;; Debug routine for developing commands.  Prints (as by `princ') the
;;; responses to the command and returns values (STATUS RESPONSES).
;;; Here's an example for printing the message sequence numbers and
;;; subjects unread messages:
#+(OR)
(multiple-value-bind (status more)
    (dbg *connection* "a" "SEARCH" "UNSEEN")
  (declare (ignore status))
  (multiple-value-bind (status more)
      (dbg *connection* "a" "FETCH" (format nil "~{~D~^,~}" (search-list (car more))) '(("ENVELOPE")))
    (declare (ignore status))
    (dolist (fetch more)
      (format t "~S: ~A~&"
              (fetch-message-sequence-number fetch)
              (second (getf (slot-value fetch 'plist) :envelope))))))
(defun dbg (connection tag &rest command)
  (mp:send-data connection (append (list tag) command))
  (do ((acc ())
       (resp (mp:parse-response connection)
             (mp:parse-response connection)))
      ((and (typep resp 'status-response)
            (equal tag (status-response-tag resp)))
       (princ resp)
       (values resp (nreverse acc)))
    (princ resp)
    (push resp acc)
    (terpri)))

(defmethod login ((conn fundamental-imap4-client) (user string) &optional password)
  (let ((tag (imap4-client-tag conn))
        (success? nil))
    (mp:message-case (conn :id tag)
        (:send-data :id "LOGIN" user (if (stringp password) password
                                         (list :password :method password :user user)))
      (imap4-protocol:ok (resp)
        (setf (slot-value conn 'connect-response) nil
              success? resp)))
    success?))

(defgeneric select (connection mailbox &optional read-only?)
  (:documentation "Select the specified mailbox.  Returns non-NIL on success.

When READ-ONLY? is non-NIL, the mailbox is selected in a read-only
fashion.")
  (:method ((conn fundamental-imap4-client) (mb string) &optional read-only?)
    (let ((tag (imap4-client-tag conn))
          (state ())
          (success?))
      (mp:message-case (conn :id tag)
          (:send-data :id (if read-only? "EXAMINE" "SELECT") mb)
        (imap4-protocol:ok (resp)
          (when (equalp (status-response-tag resp) tag)
            (setf (slot-value conn 'mailbox) state
                  success? t)))
        (imap4-protocol:no (resp)
          (when (equalp (status-response-tag resp) tag)
            (setf (slot-value conn 'mailbox) nil)))
        (imap4-protocol:exists (resp)
          (setf (getf state :exists) (ido-count resp)))
        (imap4-protocol:recent (resp)
          (setf (getf state :recent) (ido-count resp))))
      success?)))

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

(defmethod mp:send-datum ((o fundamental-imap4-client) (val list) _ &key &allow-other-keys)
  (write-string "("
                (if (slot-value o 'core::debug?) (make-broadcast-stream (core:imap4-connection-stream o) *trace-output*) (core:imap4-connection-stream o)))
  (mapcar (lambda (val) (mp:send-datum o val t)) (butlast val))
  (let ((core::%append-space% nil))
    (declare (special core::%append-space%))
    (mp:send-datum o (car (last val)) t))
  (write-string ")"
                (if (slot-value o 'core::debug?) (make-broadcast-stream (core:imap4-connection-stream o) *trace-output*) (core:imap4-connection-stream o))))

(defmethod mp:send-datum ((o fundamental-imap4-client) (val (eql :password)) _ &key method user)
  "Read a password using :METHOD.  Use :USER to notify which user password is for.

See the generic function `read-password' for acceptable values to :METHOD."
  (mp:send-datum o (read-password method :user user) _))
