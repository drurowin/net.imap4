(cl:in-package :org.drurowin.net.imap4.1)

(defgeneric imap4-client-tag (client) (:documentation "Return the next tag."))

(defgeneric imap4-client-host (client) (:documentation "Return the server hostname."))
(defgeneric imap4-client-port (client) (:documentation "Return the server port number."))
(defgeneric imap4-client-sslp (client) (:documentation "Return true when the connection uses SSL."))

(defgeneric command (connection command &rest args))

(defgeneric read-response (connection)
  (:documentation "Read an entire response from the connection."))

(defgeneric make-response (connection response tag data)
  (:documentation "Call the connection's response processor on the IMAP data."))

(defgeneric response-processor (connection response)
  (:documentation "The processor function of the response."))
