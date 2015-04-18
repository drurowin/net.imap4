(cl:in-package :org.drurowin.net.imap4.1)

(defmethod stream-write-string ((s fundamental-imap4-connection) o &optional start end)
  (stream-write-string (slot-value s 'stream) o start end))

;;;;========================================================
;;;; IMAP->lisp reader
(define-condition imap4-reader-error (reader-error)
  ()
  (:documentation "Parent class of errors involving reading IMAP4 protocol data."))

(define-condition simple-imap4-reader-error (simple-condition imap4-reader-error) ()
  (:report (lambda (c s)
             (format s "Error while reading IMAP4 protocol data~@[ from ~S~].~%"
                     (stream-error-stream c))
             (if (simple-condition-format-control c)
                 (pprint-logical-block (s nil :per-line-prefix "  ")
                   (apply #'format s (simple-condition-format-control c) (simple-condition-format-arguments c)))
                 (write-line "  Error unspecified." s)))))
(defun simple-imap4-reader-error (stream control &rest args)
  (error 'simple-imap4-reader-error :stream stream :format-control control :format-arguments args))

(defun check-ascii (stream octet)
  (when (> octet 127)
    (simple-imap4-reader-error stream "The code point ~X is not an ASCII character." octet)))

(defun check-crlf (stream last-char char)
  (when (and (eql last-char #\Return)
             (not (eql char #\Linefeed)))
    (simple-imap4-reader-error stream "The character ~@C followed a carriage return, not a line feed." char)))

(defun check-not-reserved (stream char &rest chars)
  (when (find char chars)
    (simple-imap4-reader-error stream "The character ~@C is reserved." char)))

(defgeneric skip-whitespace (stream)
  (:method ((s flexi-streams:flexi-input-stream))
    (loop (if (cl-unicode:has-binary-property (code-char (flexi-streams:peek-byte s)) "White_Space")
              (read-byte s)
              (return-from skip-whitespace nil)))))

(defmethod stream-read-line ((s fundamental-imap4-connection))
  (with-output-to-string (out)
    (do* ((octet (read-byte s) (read-byte s))
          (last-char nil char)
          (char (code-char octet) (code-char octet)))
         ((and (eql char #\Linefeed)
               (eq last-char #\Return)))
      (check-ascii s octet)
      (check-crlf s last-char char)
      (unless (eql char #\Return) (write-char char out)))))

(defgeneric read-imap4-atom (stream)
  (:method ((s flexi-streams:flexi-input-stream))
    (with-output-to-string (out)
      (do ((octet (flexi-streams:peek-byte s) (flexi-streams:peek-byte s)))
          ((member (code-char octet) '(#\Space #\Return #\Linefeed #\[ #\] #\( #\) #\{ #\})))
        (let ((char (code-char (read-byte s))))
          (check-ascii s octet)
          (check-not-reserved s char #\" #\Linefeed)
          (write-char char out))))))

(defgeneric read-imap4-quoted-string (stream)
  (:method ((s fundamental-binary-input-stream))
    (with-output-to-string (out)
      (read-byte s)
      (loop :with char = (code-char (read-byte s))
            :until (eql char #\")
            :do (check-not-reserved s char #\Return #\Linefeed)
                (write-char char out)))))

(defgeneric read-imap4-literal (stream)
  (:method ((s fundamental-binary-input-stream))
    (read-byte s)
    (let ((length
            (sequence:collect (chars)
              (do ((char (code-char (read-byte s)) (code-char (read-byte s))))
                  ((eql char #\})
                   (parse-integer (make-array (length (chars)) :element-type 'character :initial-contents (chars))))
                (chars char)))))
      (read-byte s) ;; #\Return
      (read-byte s) ;; #\Linefeed
      (let ((array (make-array length :element-type '(unsigned-byte 8))))
        (read-sequence array s)
        array))))
