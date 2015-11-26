(asdf:defsystem :org.drurowin.net.imap4.core.1
  :author "Lucien Pullen"
  :mailto "drurowin@gmail.com"
  :depends-on (:trivial-gray-streams :flexi-streams
               :closer-mop
               :org.drurowin.deflambda.1
               :org.drurowin.sequence.2)
  :components ((:module "core"
                :components ((:file "pkg")
                             (:file "ido" :depends-on ("pkg"))
                             (:file "io" :depends-on ("pkg"))))))
