(cl:in-package :cl-user)

(asdf:defsystem :org.drurowin.net.imap4.1
  :author "Lucien Pullen"
  :mailto "drurowin@gmail.com"
  :depends-on (:cl+ssl :usocket :trivial-gray-streams :closer-mop
               :cl-unicode
               :org.drurowin!guid
               :org.drurowin.deflambda.1 :org.drurowin.sequence.2
               :org.drurowin.message-processor
               :org.drurowin.io.data-streams.1 :org.drurowin.io.generic-open.1 :org.drurowin.io.message-streams.1)
  :components ((:file "package")
               (:file "common" :depends-on ("package"))
               (:file "condition" :depends-on ("package"))
               (:file "protocol" :depends-on ("package" "condition"))
               (:module "core" :depends-on ("common" "condition" "protocol")
                :components ((:file "api")
                             (:file "connection" :depends-on ("api"))
                             (:file "ido" :depends-on ("api"))
                             (:file "interact" :depends-on ("ido" "connection"))
                             (:file "standard-ido" :depends-on ("ido" "interact"))))))
