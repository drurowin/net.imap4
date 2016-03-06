;;;; IMAP4 for Common Lisp
;;;; implementation of IMAP4 capability
(in-package :org.drurowin.net.imap4.client.1)

(core:define-capability +imap4+ () "IMAP4"
  "baseline IMAP version 4")

(defgeneric status-response-tag (status-response))
(defgeneric status-response-code (status-response))
(defgeneric status-response-code-data (status-response))
(defgeneric status-response-text (status-response))

(defclass status-response ()
  ((tag :initarg :tag :reader status-response-tag)
   (code :initarg :code)
   (text :initarg :text :reader status-response-text)))

(defmethod status-response-code ((o status-response))
  (car (slot-value o 'code)))

(defmethod status-response-code-data ((o status-response))
  (cdr (slot-value o 'code)))

(defmethod print-object ((o status-response) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~@[~A ~]~S"
            (slot-value o 'tag)
            (slot-value o 'text))))

(defparameter status-response-reader
  (core:lambda/imap4 (&response r &tag tag &optional (code #\[) &text text)
    (make-instance r :tag (if (equal tag "*") nil tag) :code code :text text)))

(core:define-imap-data-object :ok (status-response)
  ()
  (:reader status-response-reader)
  (:capability +imap4+))

(core:define-imap-data-object :no (status-response)
  ()
  (:reader status-response-reader)
  (:capability +imap4+))

(core:define-imap-data-object :bad (status-response)
  ()
  (:reader status-response-reader)
  (:capability +imap4+))

(core:define-imap-data-object :bye (status-response)
  ()
  (:reader status-response-reader)
  (:capability +imap4+))

(core:define-imap-data-object :preauth (status-response)
  ()
  (:reader status-response-reader)
  (:capability +imap4+))

(core:define-imap-data-object :capability ()
  ((list :initarg :list))
  (:reader (core:lambda/imap4 (&rest list) (make-instance (core:find-ido :capability) :list list)))
  (:capability +imap4+))

(core:define-imap-data-object :flags ()
  ((list :initarg :list))
  (:reader (core:lambda/imap4 (list) (make-instance (core:find-ido :flags) :list list)))
  (:capability +imap4+))

(defmethod print-object ((o imap4-protocol::flags) s)
  (print-unreadable-object (o s :type t :identity t)
    (prin1 (format nil "~{~A~^, ~}" (slot-value o 'list)) s)))

(defgeneric ido-count (count-ido))

(defclass count-ido ()
  ((count :initarg :count :reader ido-count)))

(defmethod print-object ((o count-ido) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~:D" (slot-value o 'count))))

(core:define-imap-data-object :exists (count-ido)
  ()
  (:reader (core:lambda/imap4 (&response r &data count)
             (make-instance r :count count)))
  (:capability +imap4+))

(core:define-imap-data-object :recent (count-ido)
  ()
  (:reader (core:lambda/imap4 (&response r &data count)
             (make-instance r :count count)))
  (:capability +imap4+))

(core:define-imap-data-object :expunge ()
  ((msn :initarg :msn :reader expunge-msn))
  (:capability +imap4+)
  (:reader (core:lambda/imap4 (&data msn) (make-instance (core:find-ido :expunge) :msn (parse-integer msn)))))

(defgeneric mbox-has-attribute-p (mbox attribute))

(defclass mbox-listing ()
  ((name :initarg :name :reader mbox-name)
   (delimiter :initarg :delimiter :reader mbox-name-delimiter)
   (attr :initarg :attr))
  (:documentation "mailbox handle"))

(defmethod print-object ((o mbox-listing) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~S" (slot-value o 'name))))

(defmethod mbox-has-attribute-p ((o mbox-listing) name)
  (dolist (attr (slot-value o 'attr))
    (when (equalp attr name) (return t))))

(defmethod initialize-instance :before ((o mbox-listing) &key name delimiter attr &allow-other-keys)
  (check-type name string)
  (check-type delimiter (or null string character))
  (check-type attr list))

(defmethod initialize-instance :after ((o mbox-listing) &key delimiter attr &allow-other-keys)
  (setf (slot-value o 'delimiter)
        (typecase delimiter
          (character delimiter)
          (string (char delimiter 0))
          (null nil)))
  (setf (slot-value o 'attr)
        (remove-duplicates (mapcar (lambda (name)
                                     (core::string-case (string-upcase name)
                                       :bind it
                                       ("\\NOINFERIORS" :no-inferiors)
                                       ("\\NOSELECT" :no-select)
                                       ("\\MARKED" :marked)
                                       ("\\UNMARKED" :unmarked)
                                       (t it)))
                                   attr)
                           :test #'equalp)))

(core:define-imap-data-object :list (mbox-listing) ()
  (:capability +imap4+)
  (:reader (core:lambda/imap4 (attr delim name)
             (make-instance (core:find-ido :list) :name name :delimiter delim :attr attr))))

(core:define-imap-data-object :lsub (mbox-listing) ()
  (:capability +imap4+)
  (:reader (core:lambda/imap4 (attr delim name)
             (make-instance (core:find-ido :lsub) :name name :delimiter delim :attr attr))))

(core:define-imap-data-object :search ()
  ((list :initarg :list :reader search-list))
  (:capability +imap4+)
  (:reader (core:lambda/imap4 (&rest list) (make-instance (core:find-ido :search)
                                             :list (mapcar #'parse-integer list)))))

(defgeneric fetch-message-sequence-number (fetch))
(defgeneric get-fetch (fetch indicator))

(defun parse-fetch-part-type (type)
  (if (find type '("MESSAGE" "MULTIPART"                        #| mixed types |#
                   "TEXT" "IMAGE" "AUDIO" "VIDEO" "APPLICATION" #| atom types |#
                   "EXAMPLE" "MODEL"                            #| misc types |#)
            :test #'equalp)
      (intern (string-upcase type) :keyword)
      type))

(defun parse-fetch-part-subtype (subtype)
  (if (find subtype '(#|atom|# "PLAIN"
                      #|multipart|# "MIXED" "ALTERNATIVE" "DIGEST" "PARALLEL" "RELATED"
                      #|message|# "RFC822")
            :test #'equalp)
      (intern (string-upcase subtype) :keyword)
      subtype))

(defun parse-fetch-part-plist (plist)
  (if (equalp plist "NIL")
      ()
      (progn (assert (evenp (length plist)))
             (org.drurowin.sequence.2:collect (acc)
               (do* ((rest plist (cddr rest))
                     (key (car rest) (car rest))
                     (value (cadr rest) (cadr rest)))
                    ((null rest) (acc))
                 (core::string-case key
                   ("BOUNDARY" (acc :boundary value))
                   ("CHARSET" (acc :external-format
                                   (or (find-symbol (string-upcase value) :keyword) value)))
                   (t (acc key value))))))))

(defun parse-fetch-atompart-bodystructure (list)
  (let ((type (parse-fetch-part-type (car list))))
    (flet ((extension (rem)
             (list (let ((it (car rem) #|MD5|#))
                     (if (equalp it "NIL") nil it))
                   (let ((it (cadr rem) #|disposition|#))
                     (if (equalp it "NIL") nil
                         (destructuring-bind (type &rest plist)
                             it
                           (let ((type (core::string-case (string-upcase type)
                                         :bind type
                                         ("INLINE" :inline)
                                         ("ATTACHMENT" :attachment)
                                         (t type))))
                             (if (equalp (car plist) "NIL")
                                 type
                                 (cons type plist))))))
                   (let ((it (caddr rem) #|lang|#))
                     (if (equalp it "NIL") nil it))
                   (let ((it (cadddr rem) #|loc|#))
                     (if (equalp it "NIL") nil it)))))
      (list* type
             (parse-fetch-part-subtype (elt list 1)) ; subtype
             (parse-fetch-part-plist (elt list 2)) ; parameter list
             (elt list 3) ; ID
             (elt list 4) ; description
             (elt list 5) ; transfer encoding
             (parse-integer (elt list 6)) ; size
             (if (eql type :text)
                 (append (list (parse-integer (elt list 7)))
                         (extension (nthcdr 8 list)))
                 (extension (nthcdr 7 list)))))))

(defun parse-fetch-bodystructure-message-rfc822 (list)
  (assert (and (equalp (car list) "MESSAGE")
               (equalp (cadr list) "RFC822")))
  (list :message :rfc822
        (elt list 2)
        (elt list 3)
        (elt list 4)
        (elt list 5)                    ; transfer encoding
        (parse-integer (elt list 6))))  ; size

(defun parse-fetch-bodystructure-message (list)
  (cond ((equalp (cadr list) "RFC822")
         (parse-fetch-bodystructure-message-rfc822 list))
        (t (error "Message subtype ~A not supported." (cadr list)))))

(defun parse-fetch-multipart-bodystructure (list)
  (do* ((inner-parts ())
        (acc ())
        (rest list (cdr rest))
        (spec (car rest) (car rest))
        (inner-part-part t))
       ((null rest) (append (list :multipart)
                            (let ((acc (nreverse acc)))
                              (list (parse-fetch-part-subtype (car acc))
                                    (parse-fetch-part-plist (second acc))
                                    (if (equalp (third acc) "NIL")
                                        nil
                                        (third acc))
                                    (if (equalp (fourth acc) "NIL")
                                        nil
                                        (fourth acc))))
                            (nreverse inner-parts)))
    (if (and (consp spec)
             inner-part-part)
        (push (parse-fetch-bodystructure spec) inner-parts)
        (progn (push spec acc)
               (setf inner-part-part nil)))))

(defun parse-fetch-bodystructure (list)
  (handler-case
      (if (consp (car list))
          (parse-fetch-multipart-bodystructure list)
          (if (equalp (car list) "MESSAGE")
              (parse-fetch-bodystructure-message list)
              (parse-fetch-atompart-bodystructure list)))
    (error (c) c)))

#||
(defparameter *bodystructure*
  '((("TEXT" "PLAIN" ("CHARSET" "UTF-8") "NIL" "NIL" "7BIT" "404" "15" "NIL" "NIL" "NIL")
     ("TEXT" "HTML" ("CHARSET" "UTF-8") "NIL" "NIL" "QUOTED-PRINTABLE" "1439" "27" "NIL" "NIL" "NIL") "ALTERNATIVE"
     ("BOUNDARY" "bcaec51718652294f2050212637d") "NIL" "NIL")
    ("TEXT" "PLAIN" ("CHARSET" "us-ascii") "NIL" "NIL" "7BIT" "159" "4" "NIL" ("INLINE" "NIL") "NIL") "MIXED"
    ("BOUNDARY" "===============2406590833188767953==") "NIL" "NIL"))

(parse-fetch-bodystructure *bodystructure*)
;; => (:MULTIPART :MIXED ("BOUNDARY" "===============2406590833188767953==") NIL NIL
;;                (:MULTIPART :ALTERNATIVE ("BOUNDARY" "bcaec51718652294f2050212637d") NIL NIL
;;                            (:TEXT :PLAIN ("CHARSET" "UTF-8") "NIL" "NIL" "7BIT" 404)
;;                            (:TEXT "HTML" ("CHARSET" "UTF-8") "NIL" "NIL" "QUOTED-PRINTABLE" 1439))
;;                (:TEXT :PLAIN ("CHARSET" "us-ascii") "NIL" "NIL" "7BIT" 159))
||#

(core:define-imap-data-object :fetch ()
  ((msn :initarg :msn :reader fetch-message-sequence-number)
   (plist :initarg :plist))
  (:capability +imap4+)
  (:reader (core:lambda/imap4 (plist &response r &data msn)
             (make-instance r :msn msn
               :plist (org.drurowin.sequence.2:collect (acc)
                        (do ((rest plist (cddr rest)))
                            ((null rest) (acc))
                          (core::string-case (car rest)
                            :bind it
                            ("BODYSTRUCTURE"
                               (acc :bodystructure
                                    (parse-fetch-bodystructure (cadr rest))))
                            (("UID" "ENVELOPE")
                               (acc (intern it :keyword)
                                    (cadr rest)))
                            ("BODY"
                               (acc (cons :body (cadr rest))
                                    (caddr rest))
                               (setf rest (cdr rest))))))))))

(defmethod get-fetch ((o imap4-protocol::fetch) ind)
  (do ((rest (slot-value o 'plist) (cddr rest)))
      ((null rest))
    (when (equalp ind (car rest))
      (return-from get-fetch (cadr rest)))))

(defmethod print-object ((o imap4-protocol::fetch) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~:D ~_(~{~A~^ ~})"
            (slot-value o 'msn)
            (loop :for prop :in (slot-value o 'plist) :by #'cddr
                  :collect prop))))
