(cl:in-package :org.drurowin.net.imap4.1)

(defmethod accept-data ((s fundamental-imap4-client) (o continuation-request) &key &allow-other-keys)
  "Wait until the server sends a continuation request."
  (listen s))

;;;;========================================================
;;;; response reader
(defmethod make-response (conn response tag data)
  (let ((maybe-processor (response-processor conn response)))
    (if maybe-processor
        (funcall (data-object-processor maybe-processor) maybe-processor (imap4-connection-stream conn) tag data)
        (error "Unknown response ~A for connection ~S." response conn))))

(defmethod message-processor:parse-response ((conn fundamental-imap4-client))
  (let ((s (imap4-connection-stream conn)))
    (let ((tag (read-imap4 s)))
      (if (equal tag "+")
          (make-instance 'imap4-protocol:continuation-request :text (read-imap4-line s))
          (let ((data (let ((raw (read-imap4 s)))
                        (or (ignore-errors (parse-integer raw)) raw))))
            (if (numberp data)
                (make-response conn (read-imap4 s) tag data)
                (make-response conn data tag nil)))))))

(defmethod message-processor:handle-response ((conn fundamental-imap4-client) (resp imap4-protocol:capability))
  (when (find "IMAP4rev1" (capability-list resp) :test #'equal)
    (load-component *package* "client" "imap4rev1")))

(define-condition unhandled-continuation-request (error)
  ((text :initarg :text :initform nil :reader unhandled-continuation-request-text)
   (connection :initarg :connection :initform nil :reader unhandled-continuation-request-connection))
  (:report (lambda (c s)
             (format s "A continuation request~@[ on ~S~] was unhandled.~@[~%Text: ~A~]"
                     (unhandled-continuation-request-connection c)
                     (unhandled-continuation-request-text c)))))

(defmethod message-processor:no-applicable-handler ((conn fundamental-imap4-client) (resp imap4-protocol:+))
  (error 'unhandled-continuation-request :connection conn :text (+-text resp)))

(defmethod message-processor:send-data ((conn fundamental-imap4-client) data)
  "Encode program data to IMAP4 protocol data and send it.  The
following mini-syntax of the data is supported:

  symbol

    A symbol which identifies an IMAP data object.  For extensions, the
    transmitted string is dependent on the server.

  {string &key encoding external-format}

    Specially encode the string for transport.  The default is to use
    LATIN-1 with no special encoding.

  {NIL &key atomp}

    Encode NIL as the empty list (atomp := NIL) (default) or token NIL.

  {ido &key &allow-other-keys}

    An IMAP data object.  No keywords are defined.")

;; literal syntax
(defmethod send-data ((s fundamental-imap4-client) (o vector) &key &allow-other-keys)
  (if (typep o '(array (unsigned-byte 8) (*)))
      (progn (write-char #\{ s)
             (write (length o) :stream s :base 10 :radix nil)
             (write-char #\} s)
             (finish-output s)
             (accept-data s 'continuation-request)
             (write-sequence o s))
      (call-next-method)))

(defmethod command ((s fundamental-imap4-client) (o (eql 'capability)) &rest _)
  (declare (ignore _))
  (let ((tag (imap4-client-tag s)))
    (send-data s 'capability :tag tag)
    (do ((data '())
         (response (read-response s)))
        ((equal (response-tag response) tag)
         (values response data))
      (cond ((typep response 'capability) (push response data))
            (t (signal-message s response))))))

(defmethod send-data ((s fundamental-imap4-client) (o (eql 'capability)) &key tag)
  (write-string tag s)
  (write-char #\Space s)
  (write-string "CAPABILITY" s)
  (finish-output s))

(defmethod send-data ((s fundamental-imap4-client) (o (eql 'select)) &key tag mailbox)
  (write-string tag s)
  (write-char #\Space s)
  (write-string "SELECT" s)
  (write-char #\Space s)
  (send-data s mailbox)
  (finish-output s))

(defmethod send-data ((s fundamental-imap4-client) (o (eql 'examine)) &key tag mailbox)
  (write-string tag s)
  (write-char #\Space s)
  (write-string "EXAMINE" s)
  (write-char #\Space s)
  (send-data s mailbox)
  (finish-output s))
