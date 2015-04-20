(cl:in-package :org.drurowin.net.imap4.1)

(defmethod accept-data ((s fundamental-imap4-client) (o continuation-request) &key &allow-other-keys)
  "Wait until the server sends a continuation request."
  (listen s))

;;;;========================================================
;;;; response reader
(defmethod make-response (conn response tag data)
  (let ((maybe-processor (response-processor conn response)))
    (if maybe-processor
        (funcall (data-object-processor maybe-processor) (imap4-connection-stream conn) tag data)
        (error "Unknown response ~A for connection ~S." response conn))))

(defmethod read-response ((c fundamental-imap4-client))
  (let ((s (imap4-connection-stream c)))
    (let ((tag (read-imap4-atom s)))
      (if (equal tag "+")
          (make-instance 'continuation-request :text (read-imap4-line s))
          (let ((data (let ((raw (read-imap4 s)))
                        (or (ignore-errors (parse-integer raw)) raw))))
            (if (numberp data)
                (make-response c (read-imap4 s) tag data)
                (make-response c data tag nil)))))))

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
