(ql:quickload :cl-ppcre)
(rename-package "CL-PPCRE" "CL-PPCRE" '("PPCRE" "RE"))
(re:split "\\s+" "foo bar baz")
(re:scan "(a)*b" "xaaabd")


(with-open-file (stream "/etc/passwd")
  (let ((fields (re:split ":" (read-line stream))))
    (format t "~a~%" (nth 0 fields))
    ))
