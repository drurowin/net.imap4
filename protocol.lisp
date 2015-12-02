;;;; IMAP<->lisp serialization
;;;
;;; This file provides the low-level stream IO routines for taking IMAP
;;; data and converting them into lisp objects and vice versa.  These
;;; routines are provided as generic functions to support multiple wire
;;; types.
;;;
;;; All routines provided by the library operate on flexi streams.
(cl:in-package :org.drurowin.net.imap4.1)

;;;;========================================================
;;;; reader
;;;
;;; Convert IMAP4 protocol data into lisp objects.  Not responsible for
;;; processing the data, such as the atom NIL vs a NIL field.

;;;;========================================================
;;;; writer
;;;
;;; Convert lisp objects into IMAP protocol data.
(define-condition cons-or-atom () ())
(defun cons-or-atom ()
  "Request the caller specify how NIL should be printed.  Should invoke
the `as-cons' or `as-atom' restart (helper functions are provided by the
same name)."
  (restart-case (error 'cons-or-atom)
    (as-cons () 'cons)
    (as-atom () 'atom)))

(defun as-cons (&optional condition)
  (let ((r (find-restart 'as-cons condition)))
    (when r (invoke-restart r))))

(defun as-atom (&optional condition)
  (let ((r (find-restart 'as-atom condition)))
    (when r (invoke-restart r))))
