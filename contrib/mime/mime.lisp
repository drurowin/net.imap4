;;;; Networking
;;;; Email
;;;; Multipurpose Internet Mail Extensions
;;; Copyright (C) 2016 Lucien Pullen <drurowin@gmail.com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
(defpackage :org.drurowin.net.mail.mime
  (:use :cl))

(in-package :org.drurowin.net.mail.mime)

(defgeneric part-id (part))
(defgeneric part-name (part))
(defgeneric part-data (part))

(defclass fundamental-part ()
  ((id :initarg :id :reader part-id)
   (name :initarg :name :reader part-name)
   (headers))
  (:documentation "Parent class of MIME parts."))

(defclass singleton-part (fundamental-part)
  ((data :initarg :data :reader part-data))
  (:documentation "Parent class of non-multiparts."))

(defclass textual-part (singleton-part)
  ((external-format :initarg :external-format))
  (:documentation "Parent class of the text type."))

;;;; Text->Lispobj map
;;;;========================================================
(defvar %part-types-table% (make-hash-table :test 'equal :weakness :value :synchronized t))

(defun find-part-type (type subtype)
  (gethash (list (string-downcase type) (string-downcase subtype)) %part-types-table%))

(defun (setf find-part-type) (class type subtype)
  (check-type class fundamental-part)
  (check-type type string)
  (check-type subtype string)
  (setf (gethash (list (string-downcase type) (string-downcase subtype)) %part-types-table%) class))

(defmacro define-part-type (name direct-superclasses direct-slots &rest options)
  (check-type direct-superclasses (not null))
  (let ((types (cdr (assoc :types options)))
        (options (remove :types options :key #'car)))
    `(progn (defclass ,name ,direct-superclasses ,direct-slots ,@options)
            (eval-when (:load-toplevel :execute)
              (dolist (typespec ',types) (setf (find-part-type (car typespec) (cadr typespec)) (find-class ',name))))
            ',name)))
