;;;; IMAP4 for Common Lisp
;;;; implementation of IMAP4rev1 capability
(in-package :org.drurowin.net.imap4.client.1)

(core:define-capability +imap4-rev1+ (+imap4+) "IMAP4rev1"
  "IMAP version 4 revision 1")

(core:define-imap-data-object :status ()
  ((name :initarg :name)
   (plist :initarg :plist))
  (:capability +imap4-rev1+)
  (:reader (core:lambda/imap4 (name plist)
             (make-instance (core:find-ido :status) :name name
               :plist (do* ((acc ())
                            (plist plist (cddr plist))
                            (key (car plist) (car plist))
                            (val (cadr plist) (cadr plist)))
                           ((null plist) acc)
                        (setf (getf acc (core::string-case (string-upcase key)
                                          :bind it
                                          ("MESSAGES" :messages)
                                          ("UIDNEXT" :next-uid)
                                          ("UIDVALIDITY" :uidvalidity)
                                          ("UNSEEN" :unseen)
                                          (t (cerror "Add to data list" "Unknown STATUS data ~A." it))))
                              (parse-integer val)))))))
