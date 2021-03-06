;;;; IMAP4 for Common Lisp
(defpackage :imap4-protocol
  (:export #:ok
           #:no
           #:bad
           #:bye
           #:preauth
           #:+
           ;; server and mailbox status
           #:capability
           #:list
           #:lsub
           #:status #|revision 1|#
           #:search
           #:flags
           ;; mailbox size
           #:exists
           #:recent
           ;; message status
           #:expunge
           #:fetch))

(defpackage :org.drurowin.net.imap4.core.1
  (:documentation "Internet Message Access Protocol version 4

Core constructs shared between clients and servers.")
  (:use :cl)
  (:import-from :org.drurowin.sequence.2
                #:collect)
  (:import-from :org.drurowin.deflambda.1
                #:deflambda)
  (:local-nicknames (:generic-open "//org.drurowin:2011/io@1")
                    (:mp :org.drurowin.message-processor))
  (:export
   ;; IMAP Data Objects
   #:ido-class
   #:ido-object
   #:define-imap-data-object
   #:find-ido
   #:data-object-reader
   ;; connections
   #:imap4-connection
   #:*connection*
   #:imap4-connection-capabilities
   #:imap4-connection-stream
   ;; capabilities
   #:capability
   #:define-capability
   #:find-capability
   #:capability-inherits-from
   #:find-applicable-ido
   #:no-applicable-ido
   #:no-applicable-ido-capability
   #:no-applicable-ido-ido
   ;; API IO
   #:lambda/imap4
   ;; low-level IO
   #:imap4-protocol-error
   #:illegal-protocol-data
   #:illegal-protocol-data-datum
   #:illegal-protocol-data-context
   #:+default-imap4-readtable+
   #:read-imap4
   #:imap4-stream-dispatching-character
   #:read-imap4-atom
   #:read-imap4-quoted-string
   #:read-imap4-literal
   #:read-imap4-delimited-list #:read-imap4-list #:read-imap4-bracket-list
   #:read-imap4-line
   #:read-imap4-invalid-character
   #:read-imap4-response-end #:end-of-response
   #:slurp-whitespace))

(in-package :org.drurowin.net.imap4.core.1)
