;;;; IMAP4 for Common Lisp
;;;; basic connection
(in-package :org.drurowin.net.imap4.core.1)

(defgeneric imap4-connection-stream (connection)
  (:documentation "The protocol stream for the connection."))

(defgeneric (setf imap4-connection-stream) (value connection))

(defgeneric imap4-connection-capabilities (connection)
  (:documentation "List of capabilities the connection supports."))

(defvar *connection* nil "The current connection.")

(defclass imap4-connection (generic-open:fundamental-generic-io-stream mp:standard-message-processor)
  ((stream)
   (debug? :initarg :debug?)
   (capabilities :initarg :capabilities :reader imap4-connection-capabilities))
  (:documentation "parent class of IMAP connections")
  (:default-initargs :debug? nil))

(defmethod reinitialize-instance :after ((o imap4-connection) &key (capabilities nil capabilitiesp) &allow-other-keys)
  (when (slot-boundp o 'stream)
    (close o :abort t))
  (when capabilitiesp (setf (slot-value o 'capabilities) capabilities)))

(defmethod trivial-gray-streams:stream-finish-output ((s imap4-connection))
  (write-char #\Return (slot-value s 'stream))
  (write-char #\Linefeed (slot-value s 'stream))
  (trivial-gray-streams:stream-finish-output (slot-value s 'stream)))

(defmethod imap4-connection-stream ((o imap4-connection))
  (slot-value o 'stream))

(defmethod (setf imap4-connection-stream) (value (o imap4-connection))
  (setf (slot-value o 'stream) value))

(defmethod open-stream-p ((s imap4-connection))
  (and (slot-boundp s 'stream)
       (slot-value s 'stream)
       (open-stream-p (slot-value s 'stream))))

(defmethod generic-open:open-stream :around ((s imap4-connection) &rest _)
  (declare (ignore _))
  (unless (open-stream-p s)
    (call-next-method)
    (setf (slot-value s 'stream) (flexi-streams:make-flexi-stream (slot-value s 'stream))))
  s)

(defmethod close :around ((s imap4-connection) &rest _)
  (declare (ignore _))
  (call-next-method)
  (slot-makunbound s 'stream))

(defmethod mp:send-data :before ((s imap4-connection) _)
  (when (slot-value s 'debug?)
    (fresh-line *trace-output*)
    (write-string "> " *trace-output*)))

(defparameter %append-space% t)
(defmethod mp:send-datum :after ((s imap4-connection) _ more &key &allow-other-keys)
  (when %append-space%
    (if more
        (write-char #\Space (slot-value s 'stream))
        (trivial-gray-streams:stream-finish-output s))
    (when (slot-value s 'debug?)
      (if more (write-char #\Space *trace-output*) (terpri *trace-output*)))))

(defmethod mp:send-datum ((s imap4-connection) (o null) _ &key atomp)
  (write-string (if atomp "NIL" "()")
                (if (slot-value s 'debug?)
                    (make-broadcast-stream (slot-value s 'stream) *trace-output*)
                    (slot-value s 'stream))))

(defmethod mp:send-datum ((s imap4-connection) (o number) _ &key &allow-other-keys)
  (write o :stream (if (slot-value s 'debug?) (make-broadcast-stream *trace-output* (slot-value s 'stream))
                       (slot-value s 'stream))
           :base 10 :radix nil))

(define-condition not-atom-char () ())
(defun check-atom-char (char)
  (unless (member char
                  '#.(mapcar #'char-code
                             '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                               #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
                               #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
                               #\! #\@ #\# #\$ #\% #\^ #\& #\* #\- #\_ #\= #\+
                               #\\ #\| #\; #\: #\, #\< #\. #\> #\/ #\?)))
    (signal 'not-atom-char)))
(define-condition not-quoted-char () ())
(defun check-quoted-char (char)
  (when (or (> char 127)
            (eql (code-char char) #\"))
    (signal 'not-quoted-char)))
(defmethod mp:send-datum ((s imap4-connection) (o string) more &key external-format quotedp)
  (let ((encoded (flexi-streams:string-to-octets o :external-format (or external-format :latin1))))
    (handler-case
        (write-string (handler-case
                          (with-output-to-string (out)
                            (when quotedp (signal 'not-atom-char))
                            (dotimes (index (length encoded))
                              (let ((char (aref encoded index)))
                                (check-atom-char char)
                                (check-quoted-char char)
                                (write-char (code-char char) out))))
                        (not-atom-char ()
                          (with-output-to-string (out)
                            (write-char #\" out)
                            (dotimes (index (length encoded))
                              (let ((char (aref encoded index)))
                                (check-quoted-char char)
                                (write-char (code-char char) out)))
                            (write-char #\" out))))
                      (if (slot-value s 'debug?)
                          (make-broadcast-stream (slot-value s 'stream) *trace-output*)
                          (slot-value s 'stream)))
      (not-quoted-char ()
        (mp:send-datum s encoded more)))))
