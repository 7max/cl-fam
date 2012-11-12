;;;; package.lisp

(defpackage #:cl-fam
  (:use #:cl #:cffi #:tg)
  (:export
   ;; accessors
   :fam-connection
   :fam-request
   :fam-request-number
   :fam-filename
   :fam-hostname
   :fam-code
   :fam-fd
   ;; variables
   :*fam*
   ;; functions
   :fam-open-p
   :fam-open
   :fam-close
   :fam-init
   :fam-cancel-monitor
   ;; below two are not implemented in gamin, therefore not exporting
   ;; :fam-suspend-monitor
   ;; :fam-resume-monitor
   :fam-monitor-directory
   :fam-monitor-file
   :fam-pending-p
   :fam-next-event
   :fam-requests
   :fam-status))

