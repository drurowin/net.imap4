(in-package :org.drurowin.net.imap4.client.1)

(defvar *pinentry* "pinentry"
  "The path to the `pinentry' program.")

#+sbcl
(defmethod read-password ((o (eql :pinentry)) &key user domain)
  "Read the password using the pinentry program."
  (let* ((proc (sb-ext:run-program *pinentry* '()
                                   :search t
                                   :input :stream
                                   :output :stream
                                   :wait nil))
         (s (flexi-streams:make-flexi-stream (make-two-way-stream (sb-ext:process-output proc)
                                                                  (sb-ext:process-input proc))
                                             :external-format :utf-8))
         (alivep (sb-ext:process-alive-p proc)))
    (assert alivep)
    (unwind-protect
         (progn
           (let ((line (read-line s)))
             (assert (and (>= (length line) 2)
                          (string= line "OK" :end1 2))))
           (format s "SETDESC Password for ~:[<#unknown user>~;~:*~A~]@~:[<#unknown domain>~;~:*~A~]~%"
                   user domain)
           (finish-output s)
           (let ((line (read-line s)))
             (assert (and (>= (length line) 2)
                          (string= line "OK" :end1 2))))
           (format s "SETPROMPT Password~%")
           (finish-output s)
           (let ((line (read-line s)))
             (assert (and (>= (length line) 2)
                          (string= line "OK" :end1 2))))
           (format s "GETPIN~%")
           (finish-output s)
           (let ((line (read-line s)))
             (if (string= line "D " :end1 2)
                 (subseq line 2)
                 (if (equalp line "OK")
                     ""
                     (values nil line)))))
      (sb-ext:process-kill proc -9))))
