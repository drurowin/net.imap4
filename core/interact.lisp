(cl:in-package :org.drurowin.net.imap4.1)

(defun check-crlf (stream last-char char)
  (when (and (eql last-char #\Return)
             (not (eql char #\Linefeed)))
    (error "In ~A, the character ~@C followed a carriage return, not a line feed." stream char)
    #+(OR) ($simple-protocol-error stream "The character ~@C followed a carriage return, not a line feed." char)))

(defun check-ascii (stream octet)
  (when (> octet 127)
    (error "In ~A, the code point ~X is not an ASCII character." stream octet)
    #+(OR) (error 'ascii-error :stream stream :position (file-position stream) :octet octet)))

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

(defgeneric skip-whitespace (stream)
  (:method ((s flexi-streams:flexi-input-stream))
    (loop (if (cl-unicode:has-binary-property (code-char (flexi-streams:peek-byte s)) "White_Space")
              (read-byte s)
              (return-from skip-whitespace nil)))))
