(cl:in-package :cl-user)

(defpackage "//org.drurowin:2011/net/imap4@1"
  (#+extended-packages :require
   #-extended-packages :use
   :cl :trivial-gray-streams
        "//org.drurowin:2011/io/data-streams@1"
        "//org.drurowin:2011/io/generic-open@1"))

(in-package "//org.drurowin:2011/net/imap4@1")
