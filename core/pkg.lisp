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
   #:data-object-reader))

(in-package :org.drurowin.net.imap4.core.1)
