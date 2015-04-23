;;;; IMAP<->lisp serialization
;;;
;;; This file provides the low-level stream IO routines for taking IMAP
;;; data and converting them into lisp objects and vice versa.  These
;;; routines are provided as generic functions to support multiple wire
;;; types.
;;;
;;; All routines provided by the library operate on flexi streams.
(cl:in-package :org.drurowin.net.imap4.1)

;;;;========================================================
;;;; reader
;;;
;;; Convert IMAP4 protocol data into lisp objects.  Not responsible for
;;; processing the data, such as the atom NIL vs a NIL field.
(defun initial-readtable ()
  "The default IMAP4 readtable."
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

(defparameter *imap4-readtable* (initial-readtable))
(defgeneric read-imap4 (stream)
  (:method ((s flexi-streams:flexi-input-stream))
    (skip-whitespace s)
    (funcall (gethash (code-char (flexi-streams:peek-byte s))
                      *imap4-readtable*
                      'read-imap4-atom)
             s)))

(define-condition imap4-reader-error (imap4-error reader-error)
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

(define-condition imap4-read-dispatch-error (imap4-reader-error)
  ((char :initarg :char :initform nil :reader read-dispatch-error-char))
  (:report (lambda (c s)
             (format s "The character ~@[~@C~] is reserved during read dispatch."
                     (read-dispatch-error-char c)))))

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
    (loop (let ((char (code-char (flexi-streams:peek-byte s))))
            (if (find char '(#\Space))
                (read-byte s)
                (return-from skip-whitespace nil))))))

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
  (:method ((s flexi-streams:flexi-input-stream))
    (with-output-to-string (out)
      (read-byte s)
      (loop (let ((char (code-char (read-byte s))))
              (if (eql char #\")
                  (return)
                  (progn (check-not-reserved s char #\Return #\Linefeed)
                         (write-char char out))))))))

(defgeneric read-imap4-literal (stream)
  (:method ((s flexi-streams:flexi-input-stream))
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

(defgeneric read-imap4-line (stream)
  (:method ((s flexi-streams:flexi-input-stream))
    (with-output-to-string (out)
      (do* ((octet (read-byte s) (read-byte s))
            (last-char nil char)
            (char (code-char octet) (code-char octet)))
           ((and (eql char #\Linefeed)
                 (eq last-char #\Return)))
        (check-ascii s octet)
        (check-crlf s last-char char)
        (unless (eql char #\Return) (write-char char out))))))

(defgeneric read-imap4-delimited-list (stream endchar)
  (:method ((s flexi-streams:flexi-input-stream) (c character))
    (read-byte s)
    (sequence:collect (elts)
      (loop (if (eql (code-char (flexi-streams:peek-byte s)) c)
                (progn (read-byte s)
                       (return-from read-imap4-delimited-list (elts)))
                (elts (read-imap4 s)))))))

(defun read-imap4-list (stream) (read-imap4-delimited-list stream #\)))
(defun read-imap4-bracket-list (stream) (read-imap4-delimited-list stream #\]))

(define-condition end-of-response () ())
(defgeneric read-imap4-response-end (stream)
  (:method ((s flexi-streams:flexi-input-stream))
    (read-byte s)
    (let ((char (code-char (flexi-streams:peek-byte s))))
      (if (eql char #\Linefeed)
          (progn (read-byte s)
                 (signal 'end-of-response))
          (simple-imap4-reader-error s "A ~@C, not a line feed, was following a carriage return." char)))))

(defgeneric read-imap4-invalid-character (stream)
  (:method ((s flexi-streams:flexi-input-stream))
    (error 'imap4-read-dispatch-error :char (code-char (flexi-streams:peek-byte s)) :stream s)))
