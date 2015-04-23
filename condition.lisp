(cl:in-package :org.drurowin.net.imap4.1)

(define-condition imap4-condition () ()
  (:documentation "Parent class of conditions."))

(define-condition imap4-error (imap4-condition error) ()
  (:documentation "Parent class of errors."))
