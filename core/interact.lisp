(cl:in-package :org.drurowin.net.imap4.1)

(defmethod stream-write-string ((s fundamental-imap4-connection) o &optional start end)
  (stream-write-string (slot-value s 'stream) o start end))

;;;;========================================================
;;;; IMAP->lisp reader
(defmethod stream-read-line ((s fundamental-imap4-connection))
  (read-imap4-line (slot-value s 'stream)))

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
     (response :float t :initform (gensym "RESPONSE"))
     (donep :float t :initform (gensym "DONEP"))
     (tag :float t :initform (gensym "TAG"))
     (stream :float t :initform (gensym "STREAM"))
     (data :float t :initform (gensym "DATA"))
     (rest :float t :excludes text)
     (text :float t :excludes rest))
  `(lambda (,response ,stream ,tag ,data)
     ,@(unless (symbol-package response) `((declare (ignore ,response))))
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

(let ((proc (lambda/imap4 (&response r &tag tag &text text &optional (code #\[))
              (make-instance r :tag tag :text text :response-code code))))
  (dolist (status-response '(ok-response no-response bad-response preauth-response bye-response))
    (setf (data-object-processor status-response) proc))
  proc)

(let ((proc (lambda/imap4 (&response r attributes delimiter name)
              (sequence:collect (attr)
                (dolist (attr attributes)
                  (unless (find attr '("\\Unmarked" "\\HasNoChildren") :test #'equalp)
                    (attr (cond ((equalp attr "\\Noinferiors") :no-inferiors)
                                ((equalp attr "\\Noselect") :no-select)
                                ((equalp attr "\\Marked") :marked)
                                ((equalp attr "\\HasChildren") :has-children)
                                ((equalp attr "\\Drafts") :drafts)
                                ((equalp attr "\\Sent") :sent)
                                ((equalp attr "\\Junk") :junk)
                                ((equalp attr "\\Trash") :trash)
                                (t attr)))))
                (make-instance r :name name :delimiter delimiter :attributes (attr))))))
  (dolist (response '(lsub list-response))
    (setf (data-object-processor response) proc)))
