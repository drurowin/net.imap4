(in-package :org.drurowin.net.imap4.client.1)

(defgeneric login (connection user &optional password)
  (:documentation "Authenticate using the LOGIN(6.2.3) command.

If the password is a symbol, the password is obtained by invoking
`read-password' with the password as the method."))
