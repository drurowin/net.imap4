(cl:in-package :org.drurowin.net.imap4.1)

(defgeneric register-imap4-data-object (connection data-object &optional imap-name)
  (:documentation "Add the data object as a recognized data object for the connection.

If IMAP-NAME is specified, then that name overrides the default name."))

(defgeneric data-object-imap-name (data-object connection)
  (:documentation "Return the name of the data object when printed to CONNECTION.

When CONNECTION is NIL, the program-supplied name should be returned."))

(defun data-object-imap-name* (data-object &optional connection)
  "Convenience function for calling `data-object-imap-name'."
  (data-object-imap-name data-object connection))
