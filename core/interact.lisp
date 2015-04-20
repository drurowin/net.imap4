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
    (loop (let ((char (code-char (flexi-streams:peek-byte s))))
            (if (find char '(#\Space))
                (read-byte s)
                (return-from skip-whitespace nil))))))

(defgeneric read-imap4-line (stream)
  (:method ((s fundamental-binary-input-stream))
    (with-output-to-string (out)
      (do* ((octet (read-byte s) (read-byte s))
            (last-char nil char)
            (char (code-char octet) (code-char octet)))
           ((and (eql char #\Linefeed)
                 (eq last-char #\Return)))
        (check-ascii s octet)
        (check-crlf s last-char char)
        (unless (eql char #\Return) (write-char char out))))))

(defmethod stream-read-line ((s fundamental-imap4-connection))
  (read-imap4-line (slot-value s 'stream)))

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
    (simple-imap4-reader-error s "The character ~@C is reserved during read dispatch."
                               (code-char (flexi-streams:peek-byte s)))))

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

(deflambda lambda/imap4 body
    ((required :default t :repeatable nil :keyword nil :rest t)
     (optional :repeatable nil :rest t)
     (donep :float t :initform (gensym "DONEP"))
     (tag :float t :initform (gensym "TAG"))
     (stream :float t :initform (gensym "STREAM"))
     (data :float t :initform (gensym "DATA"))
     (rest :float t :excludes text)
     (text :float t :excludes rest))
  `(lambda (,stream ,tag ,data)
     ,@(unless (symbol-package tag) `((declare (ignore ,tag))))
     ,@(unless (symbol-package data) `((declare (ignore ,data))))
     (let* ((,donep nil)
            ,@(mapcar (lambda (binding)
                        `(,binding (if (not ,donep)
                                       (handler-case (read-imap4 ,stream)
                                         (end-of-response ()
                                           (setf ,donep t)
                                           (simple-imap4-reader-error ,stream "The response ended while reading the ~A required argument." ',binding)))
                                       (simple-imap4-reader-error ,stream "The response ended before reading the ~A required argument." ',binding))))
                      required)
            ,@(mapcar (lambda (optional)
                        `(,(if (atom optional) optional (car optional))
                          (unless ,donep
                            (handler-case
                                ,(if (consp optional)
                                     `(progn (skip-whitespace ,stream)
                                             (when (eql (code-char (flexi-streams:peek-byte ,stream)) ,(cadr optional))
                                               (read ,stream)))
                                     `(read-imap4 ,stream))
                              (end-of-response () (setf ,donep t))))))
                      optional)
            ,@(when rest `((,rest (unless ,donep
                                      (sequence:collect (data)
                                        (handler-case (loop (data (read-imap4 ,stream)))
                                          (end-of-response ()
                                            (setf ,donep t)
                                            (data))))))))
            ,@(when text `((,text (unless ,donep
                                    (prog2
                                        (skip-whitespace ,stream)
                                        (read-imap4-line ,stream)
                                      (setf ,donep t)))))))
       ,@(unless (symbol-package donep)
           `((unless ,donep
               (handler-case
                   (progn (read-imap4 ,stream)
                          (simple-imap4-reader-error ,stream "Response processing finished but data remains."))
                 (end-of-response ())))))
       ,@body)))

(defmacro define-response-processor (data-object lambda-list &body body)
  `(setf (data-object-processor ',data-object)
         (lambda/imap4 ,lambda-list ,@body)))

(define-response-processor capability (imaprev &rest extras)
  (make-instance 'capability :imaprev imaprev :extras extras))
