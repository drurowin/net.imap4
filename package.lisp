(cl:in-package :cl-user)

(defpackage :org.drurowin.net.imap4.1
  (#+extended-packages :require
   #-extended-packages :use
   :cl :trivial-gray-streams
   "//org.drurowin:2011/io@1"
   "//org.drurowin:2011/io/message-streams@1"
   "//org.drurowin:2011/io/data-streams@1"
   :org.drurowin.deflambda.1)
  (:local-nicknames (:sequence :org.drurowin.sequence.2)))

(in-package :org.drurowin.net.imap4.1)
