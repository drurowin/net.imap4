(cl:in-package :org.drurowin.net.imap4.1)

(defgeneric imap4-client-tag (client) (:documentation "Return the next tag."))

(defgeneric imap4-client-host (client) (:documentation "Return the server hostname."))
(defgeneric imap4-client-port (client) (:documentation "Return the server port number."))
(defgeneric imap4-client-sslp (client) (:documentation "Return true when the connection uses SSL."))

(defgeneric command (connection command &rest args))
