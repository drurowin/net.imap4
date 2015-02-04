(cl:in-package "//org.drurowin:2011/net/imap4@1")

(defclass fundamental-imap4-connection (fundamental-data-io-stream)
  ((stream))
  (:documentation "Parent class of IMAP connections."))

(defmethod data-input-stream-stream ((s fundamental-imap4-connection))
  (slot-value s 'stream))

(defmethod data-output-stream-stream ((s fundamental-imap4-connection))
  (slot-value s 'stream))

(defmethod stream-finish-output ((s fundamental-imap4-connection))
  (write-char #\Return s)
  (write-char #\Linefeed s)
  (call-next-method))

(defmethod imap4-connection-stream ((o fundamental-imap4-connection))
  (slot-value o 'stream))

(defmethod (setf imap4-connection-stream) (value (o fundamental-imap4-connection))
  (setf (slot-value o 'stream) value))

(defmethod open-stream-p ((s fundamental-imap4-connection))
  (and (slot-boundp s 'stream)
       (slot-value s 'stream)
       (open-stream-p (slot-value s 'stream))))

(defmethod open-stream :around ((s fundamental-imap4-connection) &rest _)
  (declare (ignore _))
  (unless (open-stream-p s)
    (call-next-method)
    (setf (slot-value s 'stream) (flexi-streams:make-flexi-stream (slot-value s 'stream))))
  s)

(defmethod close-stream :around ((s fundamental-imap4-connection) &rest _)
  (declare (ignore _))
  (call-next-method)
  (slot-makunbound s 'stream))

(defmethod send-data ((s fundamental-imap4-connection) (o number) &rest _)
  (declare (ignore _))
  (write o :stream (slot-value s 'stream) :base 10 :radix nil))

(define-condition not-atom-char () ())

(defun check-atom-char (char)
  (unless (member char
                  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
                    #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
                    #\! #\@ #\# #\$ #\% #\^ #\& #\* #\- #\_ #\= #\+
                    #\\ #\| #\; #\: #\, #\< #\. #\> #\/ #\?))
    (signal 'not-atom-char)))

(define-condition not-quoted-char () ())

(defun check-quoted-char (char)
  (when (or (> (char-code char) 127)
            (eql char #\"))
    (signal 'not-quoted-char)))

(defmethod send-data ((s fundamental-imap4-connection) (o string) &key external-format)
  "When the string is not composed of ASCII characters or contains
certain control characters and \", it is converted to an octet vector as
EXTERNAL-FORMAT and printed as a literal."
  (handler-case
      (write-string (handler-case
                        (with-output-to-string (out)
                          (dotimes (index (length o))
                            (let ((char (char o index)))
                              (check-atom-char char)
                              (check-quoted-char char)
                              (write-char char out))))
                      (not-atom-char ()
                        (with-output-to-string (out)
                          (write-char #\" out)
                          (dotimes (index (length o))
                            (let ((char (char o index)))
                              (check-quoted-char char)
                              (write-char char out)))
                          (write-char #\" out))))
                    s)
    (not-quoted-char ()
      (send-data s (flexi-streams:string-to-octets o :external-format (or external-format :latin1))))))
