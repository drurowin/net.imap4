(asdf:defsystem :org.drurowin.net.imap4.client.1
  :author "Lucien Pullen"
  :mailto "drurowin@gmail.com"
  :depends-on (:cl+ssl
               :usocket
               :org.drurowin.io.generic-open.1
               :org.drurowin.message-processor
               :org.drurowin.net.imap4.core.1
               :org.drurowin.net.mail.mime)
  :components ((:module "client"
                :components ((:file "pkg")
                             (:file "command" :depends-on ("pkg"))
                             (:file "imap4-capability" :depends-on ("pkg"))
                             (:file "imap4rev1-capability" :depends-on ("imap4-capability"))
                             (:file "connection" :depends-on ("command" "imap4rev1-capability"))
                             (:file "password" :depends-on ("connection"))))))
