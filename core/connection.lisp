;;;; IMAP4 for Common Lisp
;;;; basic connection
(in-package :org.drurowin.net.imap4.core.1)

(defgeneric imap4-connection-stream (connection)
  (:documentation "The protocol stream for the connection."))

(defgeneric (setf imap4-connection-stream) (value connection))

(defgeneric write-imap4-object (object connection &key &allow-other-keys)
  (:documentation "Write the object as protocol data to the connection."))

(defvar *connection* nil "The current connection.")

(defclass imap4-connection (generic-open:fundamental-generic-io-stream message-processor:standard-message-processor)
  ((stream))
  (:documentation "parent class of IMAP connections"))

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

(defmethod write-imap4-object ((o number) (s imap4-connection) &key &allow-other-keys)
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
(defmethod write-imap4-object ((o string) (s imap4-connection) &key external-format)
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
                    (slot-value s 'stream))
    (not-quoted-char ()
      (write-imap4-object (flexi-streams:string-to-octets o :external-format (or external-format :latin1)) s))))

(defmethod message-processor:send-data ((proc imap4-connection) (data list))
  "Accept a list of program objects to send to the connection as a single
command.

Each program object may be either the object, to send it using the
default options, or the list (object . options-plist) to control the
sending options."
  (do* ((more data (cdr more))
        (datum (car more) (car more)))
       ((null more) data)
    (if (consp datum)
        (apply 'write-imap4-object (car datum) proc (cdr datum))
        (write-imap4-object datum proc))
    (if (cdr more)
        (write-string " " (slot-value proc 'stream))
        (trivial-gray-streams:stream-finish-output proc))))
