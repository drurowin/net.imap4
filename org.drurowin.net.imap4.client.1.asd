(asdf:defsystem :org.drurowin.net.imap4.client.1
  :author "Lucien Pullen"
  :mailto "drurowin@gmail.com"
  :depends-on (:org.drurowin.net.imap4.core.1)
  :components ((:module ("client")
                :components ((:file "pkg")))))
