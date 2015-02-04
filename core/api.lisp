(cl:in-package "//org.drurowin:2011/net/imap4@1")

(defparameter *connection* nil "The current connection.")

(defgeneric imap4-connection-stream (connection)
  (:documentation "Return the underlying stream of CONNECTION."))

(defgeneric (setf imap4-connection-stream) (value connection)
  (:documentation "Set the underlying stream of CONNECTION to VALUE."))

(defgeneric register-imap4-data-object (connection data-object &optional imap-name)
  (:documentation "Add the data object as a recognized data object for the connection.

If IMAP-NAME is specified, then that name overrides the default name."))

(defgeneric data-object-imap-name (data-object connection)
  (:documentation "Return the name of the data object when printed to CONNECTION.

When CONNECTION is NIL, the program-supplied name should be returned."))

(defun data-object-imap-name* (data-object &optional connection)
  "Convenience function for calling `data-object-imap-name'."
  (data-object-imap-name data-object connection))
