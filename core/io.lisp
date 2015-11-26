;;;; IMAP4 for Common Lisp
;;;; readtable-based protocol reading and program object serialization
(in-package :org.drurowin.net.imap4.core.1)

;;;;========================================================
;;;; shared
(define-condition imap4-protocol-error (stream-error) ()
  (:documentation "parent class of errors involving the IMAP4 protocol"))

(define-condition simple-imap4-protocol-error (imap4-protocol-error simple-condition) ()
  (:report (lambda (c s)
             (format s "IMAP4 protocol error on ~S:~@[~%~%~?~]"
                     (stream-error-stream c)
                     (simple-condition-format-control c)
                     (simple-condition-format-arguments c)))))
(defun simple-imap4-protocol-error (stream control &rest args)
  (error 'simple-imap4-protocol-error :stream stream :format-control control :format-arguments args))

(define-condition illegal-protocol-data (imap4-protocol-error)
  ((datum :initarg :datum :initform nil :reader illegal-protocol-data-datum)
   (context :initarg :context :initform nil :reader illegal-protocol-data-context))
  (:documentation "datum not allowed")
  (:report (lambda (c s)
             (let ((datum (illegal-protocol-data-datum c))
                   (context (illegal-protocol-data-context c)))
               (format s "IMAP4 protocol error on ~S:~%~%The datum~@[ ~S~] is not ~?"
                       (stream-error-stream c)
                       datum
                       (if context (car context) "allowed in the current reader context")
                       (cdr context))))))
(defun illegal-protocol-data (stream datum &optional context-control &rest context-args)
  (error 'illegal-protocol-data :stream stream :datum datum
                                :context (if context-control (list* context-control context-args))))

(defun check-ascii (stream octet)
  (when (> octet 127)
    (illegal-protocol-data stream (code-char octet) "an ASCII character")))

(defun check-crlf (stream last-char char)
  (when (and (eql last-char #\Return)
             (not (eql char #\Linefeed)))
    (illegal-protocol-data stream char "a line feed")))

(defun check-not-reserved (stream char &rest chars)
  (when (find char chars)
    (illegal-protocol-data stream char)))

;;;;========================================================
;;;; reader
(defgeneric read-imap4-atom (stream))
(defgeneric read-imap4-quoted-string (stream))
(defgeneric read-imap4-literal (stream))
(defgeneric read-imap4-delimited-list (stream end-character))
(defgeneric read-imap4-line (stream))
(defgeneric read-imap4-invalid-character (stream))
(defgeneric read-imap4-response-end (stream))
(defgeneric slurp-whitespace (stream))
(defgeneric read-imap4 (stream))
(defgeneric imap4-stream-dispatching-character (stream)
  (:documentation "The character that the IMAP reader will dispatch on.

Ignores whitespace.  The character is not consumed."))

(defun default-imap4-readtable ()
  (let ((table (make-hash-table)))
    (setf (gethash #\" table) 'read-imap4-quoted-string
          (gethash #\{ table) 'read-imap4-literal
          (gethash #\} table) 'read-imap4-invalid-character
          (gethash #\( table) 'read-imap4-list
          (gethash #\) table) 'read-imap4-invalid-character
          (gethash #\[ table) 'read-imap4-bracket-list
          (gethash #\] table) 'read-imap4-invalid-character
          (gethash #\Return table) 'read-imap4-response-end
          (gethash #\Linefeed table) 'read-imap4-invalid-character)
    table))

(defparameter +default-imap4-readtable+ (default-imap4-readtable))
;;;;--------------------------------------------------------
;;;; standard
(defmethod read-imap4 ((s flexi-streams:flexi-input-stream))
  (funcall (gethash (imap4-stream-dispatching-character s)
                    +default-imap4-readtable+
                    'read-imap4-atom)
           s))

(defmethod imap4-stream-dispatching-character ((s flexi-streams:flexi-input-stream))
  (slurp-whitespace s)
  (code-char (flexi-streams:peek-byte s)))

(defmethod slurp-whitespace ((s flexi-streams:flexi-input-stream))
  (loop (let ((char (code-char (flexi-streams:peek-byte s))))
          (if (find char '(#\Space))
              (read-byte s)
              (return-from slurp-whitespace nil)))))

(defmethod read-imap4-atom ((s flexi-streams:flexi-input-stream))
  (with-output-to-string (out)
    (do ((octet (flexi-streams:peek-byte s) (flexi-streams:peek-byte s)))
        ((member (code-char octet) '(#\Space #\Return #\Linefeed #\[ #\] #\( #\) #\{ #\})))
      (let ((char (code-char (read-byte s))))
        (check-ascii s octet)
        (check-not-reserved s char #\" #\Linefeed)
        (write-char char out)))))

(defmethod read-imap4-quoted-string ((s flexi-streams:flexi-input-stream))
  (with-output-to-string (out)
    (read-byte s)
    (loop (let ((char (code-char (read-byte s))))
            (if (eql char #\")
                (return)
                (progn (check-not-reserved s char #\Return #\Linefeed)
                       (write-char char out)))))))

(defmethod read-imap4-literal ((s flexi-streams:flexi-input-stream))
  (read-byte s)
  (let ((length
          (collect (chars)
            (do ((char (code-char (read-byte s)) (code-char (read-byte s))))
                ((eql char #\})
                 (parse-integer (make-array (length (chars)) :element-type 'character :initial-contents (chars))))
              (chars char)))))
    (read-byte s) ;; #\Return
    (read-byte s) ;; #\Linefeed
    (let ((array (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence array s)
      array)))

(defmethod read-imap4-delimited-list ((s flexi-streams:flexi-input-stream) (c character))
  (read-byte s)
  (collect (elts)
    (loop (if (eql (code-char (flexi-streams:peek-byte s)) c)
              (progn (read-byte s)
                     (return-from read-imap4-delimited-list (elts)))
              (elts (read-imap4 s))))))

(defun read-imap4-list (stream) (read-imap4-delimited-list stream #\)))
(defun read-imap4-bracket-list (stream) (read-imap4-delimited-list stream #\]))

(defmethod read-imap4-line ((s flexi-streams:flexi-input-stream))
  (with-output-to-string (out)
    (do* ((octet (read-byte s) (read-byte s))
          (last-char nil char)
          (char (code-char octet) (code-char octet)))
         ((and (eql char #\Linefeed)
               (eq last-char #\Return)))
      (check-ascii s octet)
      (check-crlf s last-char char)
      (unless (eql char #\Return) (write-char char out)))))

(defmethod read-imap4-invalid-character ((s flexi-streams:flexi-input-stream))
  (illegal-protocol-data s (code-char (flexi-streams:peek-byte s)) "allowed during read dispatch"))

(define-condition end-of-response () ())

(defmethod read-imap4-response-end ((s flexi-streams:flexi-input-stream))
  (read-byte s)
  (let ((char (code-char (flexi-streams:peek-byte s))))
    (if (eql char #\Linefeed)
        (progn (read-byte s)
               (signal 'end-of-response))
        (illegal-protocol-data s char "a line feed after a carriage return"))))
