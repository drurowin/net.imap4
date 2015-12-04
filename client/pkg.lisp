;;;; IMAP4 for Common Lisp
;;;; client implementation
(defpackage :org.drurowin.net.imap4.client.1
  (:documentation "Internet Message Access Protocol version 4

Client implementation.")
  (:use :cl)
  (:local-nicknames (:core :org.drurowin.net.imap4.core.1)
                    (:generic-open "//org.drurowin:2011/io@1")
                    (:mp :org.drurowin.message-processor)))
