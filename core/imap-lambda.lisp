;;;; IMAP4 for Common Lisp
;;;; protocol reader abstraction to function parameters
(in-package :org.drurowin.net.imap4.core.1)

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
                                           (simple-imap4-protocol-error ,stream "The response ended while reading the ~A required argument." ',binding)))
                                       (simple-imap4-protocol-error ,stream "The response ended before reading the ~A required argument." ',binding))))
                      required)
            ,@(mapcar (lambda (optional)
                        `(,(if (atom optional) optional (car optional))
                          (unless ,donep
                            (handler-case
                                ,(if (consp optional)
                                     `(when (eql (imap4-stream-dispatching-character ,stream) ,(cadr optional))
                                        (read-imap4 ,stream))
                                     `(read-imap4 ,stream))
                              (end-of-response () (setf ,donep t))))))
                      optional)
            ,@(when rest `((,rest (unless ,donep
                                      (collect (data)
                                        (handler-case (loop (data (read-imap4 ,stream)))
                                          (end-of-response ()
                                            (setf ,donep t)
                                            (data))))))))
            ,@(when text `((,text (unless ,donep
                                    (prog2
                                        (slurp-whitespace ,stream)
                                        (read-imap4-line ,stream)
                                      (setf ,donep t)))))))
       ,@(unless (symbol-package donep)
           `((unless ,donep
               (handler-case
                   (progn (read-imap4 ,stream)
                          (simple-imap4-protocol-error ,stream "Response processing finished but data remains."))
                 (end-of-response ())))))
       ,@body)))
