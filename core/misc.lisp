;;;; IMAP4 for Common Lisp
;;;; miscellaneous stuff
(in-package :org.drurowin.net.imap4.core.1)

(defvar %indentation% (make-hash-table))
(defmacro indentation (symbol spec)
  `(eval-when (:load-toplevel :execute)
     (setf (gethash ',symbol %indentation%) ',spec)))

#+swank (pushnew %indentation% swank::*application-hints-tables*)

(indentation string-case (as case))
(defmacro string-case (form &rest clauses)
  "Evaluate the FORM and test (via EQUAL) against the CAR of the clauses.
The first matching clause is evaluated as by PROGN.

If the first clause is the keyword :BIND, the second clause names the
variable to bind the FORM to (default is GENSYM'd) and the clauses
follow the binding name."
  (let ((gform (if (eql (car clauses) :bind)
                   (cadr clauses)
                   (gensym "FORM")))
        (clauses (if (eql (car clauses) :bind)
                     (cddr clauses)
                     clauses)))
    `(let ((,gform ,form))
       (cond ,@(mapcar (lambda (clause)
                         (let ((test (car clause))
                               (result (cdr clause)))
                           (if (eql test t)
                               `(t ,@result)
                               (if (atom test)
                                   `((equal ,gform ,test) ,@result)
                                   `((find ,gform (list ,@test) :test #'equal) ,@result)))))
                       clauses)))))
