(cl:in-package "//org.drurowin:2011/net/imap4@1")

(defmethod accept-data ((s fundamental-imap4-client) (o continuation-request) &key &allow-other-keys)
  "Wait until the server sends a continuation request."
  (listen s))

;; literal syntax
(defmethod send-data ((s fundamental-imap4-client) (o vector) &key &allow-other-keys)
  (check-type o (simple-array (unsigned-byte 8) (*)))
  (write-char #\{ s)
  (write (length o) :stream s :base 10 :radix nil)
  (write-char #\} s)
  (finish-output s)
  (accept-data s 'continuation-request)
  (write-sequence o s))

(defmethod send-data ((s fundamental-imap4-client) (o (eql 'capability)) &key tag)
  (write-string tag s)
  (write-char #\Space s)
  (write-string "CAPABILITY" s)
  (finish-output s))
