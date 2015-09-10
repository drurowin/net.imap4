(cl:in-package :cl-user)

(defpackage :org.drurowin.net.imap4.1
  (#+extended-packages :require
   #-extended-packages :use
   :cl :trivial-gray-streams
   "//org.drurowin:2011/io@1"
   "//org.drurowin:2011/io/message-streams@1"
   "//org.drurowin:2011/io/data-streams@1"
   :org.drurowin.deflambda.1)
  (:local-nicknames (:sequence :org.drurowin.sequence.2)
                    (:message-processor :org.drurowin.message-processor)))

(defpackage :imap4-protocol
  (:documentation "The defined names in IMAP4.")
  (:export #:ok
           #:no
           #:bad
           #:bye
           #:preauth
           ;;; Commands
           ;; any state
           #:noop
           #:logout
           ;; not authenticated
           #:starttls
           #:authenticate
           #:login
           ;; authenticated
           #:select
           #:examine
           #:create
           #:delete
           #:rename
           #:subscribe
           #:unsubscribe
           #:append
           ;; selected
           #:check
           #:close
           #:store
           #:copy
           #:uid
           ;;; Responses (that are also commands sometimes)
           #:continuation-request
           ;; server and mailbox status
           #:capability
           #:list
           #:lsub
           #:status
           #:search
           #:flags
           ;; mailbox size
           #:exists
           #:recent
           ;; message status
           #:expunge
           #:fetch))

(in-package :org.drurowin.net.imap4.1)
