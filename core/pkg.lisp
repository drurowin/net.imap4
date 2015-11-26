;;;; IMAP4 for Common Lisp
(defpackage :org.drurowin.net.imap4.core.1
  (:documentation "Internet Message Access Protocol version 4

Core constructs shared between clients and servers.")
  (:use :cl)
  (:import-from :org.drurowin.sequence.2
                #:collect)
  (:export
   ;; IMAP Data Objects
   #:ido-class
   #:ido-object
   #:define-imap-data-object
   #:find-ido
   #:data-object-reader
   ;; low-level IO
   #:imap4-protocol-error
   #:illegal-protocol-data
   #:illegal-protocol-data-datum
   #:illegal-protocol-data-context
   #:+default-imap4-readtable+
   #:read-imap4
   #:read-imap4-atom
   #:read-imap4-quoted-string
   #:read-imap4-literal
   #:read-imap4-delimited-list #:read-imap4-list #:read-imap4-bracket-list
   #:read-imap4-line
   #:read-imap4-invalid-character
   #:read-imap4-response-end #:end-of-response
   #:slurp-whitespace))

(in-package :org.drurowin.net.imap4.core.1)
