;;;; cl-fam.asd

(eval-when (:load-toplevel :execute)
  (asdf:load-system :cffi-grovel))

(asdf:defsystem #:cl-fam
  :serial t
  :description "Wraps libfam (File access monitor) API. Needs gamin-devel or fam-devel package"
  :author "Max Mikhanosha <max@openchat.com>"
  :license "Apache"
  :depends-on (#:cffi #:cffi-grovel #:trivial-garbage)
  :defsystem-depends-on (#:cffi-grovel)
  :components ((:file "package")
               (cffi-grovel:grovel-file "fam-grovel")
               (:file "cl-fam")))

