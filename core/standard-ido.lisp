;;;; IMAP4 for Common Lisp
;;;; IMAP Data Objects
;;;; the standard objects
(in-package :org.drurowin.net.imap4.1)

(defgeneric status-response-tag (status-response)
  (:documentation "The text identifier of the sent command."))

(defgeneric status-response-code (status-response))

(defgeneric status-response-code-parameters (status-response))

(defgeneric status-response-text (status-response)
  (:documentation "Human readable purpose of status response."))

(defclass status-response (ido-object)
  ((tag :initarg :tag :reader status-response-tag)
   (code :initarg :code)
   (text :initarg :text :reader status-response-text)))

(defmethod status-response-code ((o status-response))
  (with-slots (code) o
    (if (consp code) (car code) code)))

(defmethod status-response-code-parameters ((o status-response))
  (with-slots (code) o
    (if (consp code) (cdr code) nil)))

(defparameter %status-response-reader%
  (lambda/imap4 (&response r &tag tag &text text &optional (code #\[))
    (make-instance r :tag tag :text text
      :code (when code
              (string-case (car code)
                :bind it
                ("PERMANENTFLAGS"
                   (list* :permanent-flags (mapcar (lambda (option)
                                                     (assert (not (zerop (length option))))
                                                     (cond ((equal option "\\*") :allow-other-keys)
                                                           ((eql (char tag 0) #\\)
                                                            (or (find-symbol (string-upcase (subseq option 1)) :keyword)
                                                                option))
                                                           (t option)))
                                                   (cadr code))))
                (("ALERT" "PARSE" "READ-ONLY" "READ-WRITE" "TRYCREATE")
                   (intern (string-upcase it) :keyword))
                (t code))))))

(define-imap-data-object :ok (status-response) ()
  (:documentation "command success.  Used as a server greeting when the server is ready to
authenticate the client.")
  (:reader %status-response-reader%))

(define-imap-data-object :bad (status-response) ()
  (:documentation "protocol error")
  (:reader %status-response-reader%))

(define-imap-data-object :no (status-response) ()
  (:documentation "command failure")
  (:reader %status-response-reader%))

(define-imap-data-object :preath (status-response) ()
  (:documentation "Used as the server greeting when authentication is provided outside of
IMAP.")
  (:reader %status-response-reader%))

(define-imap-data-object :bye (status-response) ()
  (:documentation "logging out.  Used as a server greeting when the server is not accepting
connections.")
  (:reader %status-response-reader%))

(defgeneric capability-listing (capability)
  (:documentation "A list of strings representing the capabilities of the server."))

(define-imap-data-object :capability ()
  ((listing :initarg :listing :reader capability-listing))
  (:reader (lambda/imap4 (&rest text)
             (make-instance 'imap4-protocol:capability :listing text))))

(defgeneric mailbox-listing-mailbox (mailbox-listing)
  (:documentation "The mailbox's name."))

(defgeneric mailbox-listing-delimiter (mailbox-listing)
  (:documentation "The mailbox's name path separator."))

(defgeneric mailbox-listing-attributes (mailbox-listing)
  (:documentation "A list of options for the mailbox."))

(defclass mailbox-listing ()
  ((mailbox :initarg :mailboxes :reader mailbox-listing-mailbox)
   (delimiter :initarg :delimiter :reader mailbox-listing-delimiter)
   (attributes :initarg :attributes :reader mailbox-listing-attributes)))

(let ((proc (lambda/imap4 (&response r attr hier name)
              (make-instance r
                :mailbox name
                :delimiter (if (equalp hier "NIL")
                               nil
                               (char hier 0))
                :attributes (sequence:collect (attr)
                              (dolist (name attr (attr))
                                (unless (find name '("\\Unmarked" "\\HasNoChildren") :test #'equalp)
                                  (attr (string-case (string-downcase name)
                                          ("\\noinferiors" :no-inferiors)
                                          ("\\noselect" :no-select)
                                          ("\\marked" :marked)
                                          ("\\haschildren" :has-children)
                                          ("\\drafts" :drafts)
                                          ("\\sent" :sent)
                                          ("\\junk" :junk)
                                          ("\\trash" :trash)
                                          (t name))))))))))
  (define-imap-data-object :lsub (mailbox-listing) ()
    (:reader proc))
  (define-imap-data-object :list (mailbox-listing) ()
    (:reader proc)))
