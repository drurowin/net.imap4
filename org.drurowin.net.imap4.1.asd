(cl:in-package :cl-user)

(asdf:defsystem :org.drurowin.net.imap4.1
  :author "Lucien Pullen"
  :mailto "drurowin@gmail.com"
  :depends-on (:cl+ssl :usocket :trivial-gray-streams :closer-mop
               :org.drurowin.ext.deflambda :org.drurowin.io.data-streams.1 :org.drurowin.io.generic-open.1 :org.drurowin.sequence)
  :components ((:file "package")
               (:module "core" :depends-on ("package")
                :components ((:file "api")
                             (:file "connection" :depends-on ("api"))
                             (:file "ido" :depends-on ("api"))
                             (:file "interact" :depends-on ("ido" "connection"))))
               (:module "client" :depends-on ("package")
                :components ((:file "api")
                             (:file "connection" :depends-on ("api"))
                             (:file "interact" :depends-on ("connection"))))))
