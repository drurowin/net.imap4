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
(defgeneric skip-whitespace (stream)
  (:method ((s flexi-streams:flexi-input-stream))
    (loop (if (cl-unicode:has-binary-property (code-char (flexi-streams:peek-byte s)) "White_Space")
              (read-byte s)
              (return-from skip-whitespace nil)))))
