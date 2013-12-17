;;;; cl-fam.lisp

(in-package #:cl-fam)

;;; "cl-fam" goes here. Hacks and glory await!

;;; define the library
(define-foreign-library libfam
  (t "libfam.so"))

(use-foreign-library libfam)

;;; Low level interface, not exported
(defcvar ("FAMErrno" %fam-errno) :int)

(defcfun ("FAMOpen2" %fam-open-2) :int
  (conn (:pointer (:struct famconnection)))
  (app-name :string))

(defcfun ("FAMClose" %fam-close) :int
  (conn (:pointer (:struct famconnection))))

(defcfun ("FAMMonitorDirectory" %fam-monitor-directory) :int
  (conn (:pointer (:struct famconnection)))
  (filename :string)
  (req (:pointer (:struct famrequest)))
  (userData :pointer))

(defcfun ("FAMMonitorFile" %fam-monitor-file) :int
  (conn (:pointer (:struct famconnection)))
  (filename :string)
  (req (:pointer (:struct famrequest)))
  (userData :pointer))

(defcfun ("FAMCancelMonitor" %fam-cancel-monitor) :int
  (conn (:pointer (:struct famconnection)))
  (req (:pointer (:struct famrequest))))

(defcfun ("FAMSuspendMonitor" %fam-suspend-monitor) :int
  (conn (:pointer (:struct famconnection)))
  (req (:pointer (:struct famrequest))))

(defcfun ("FAMResumeMonitor" %fam-resume-monitor) :int
  (conn (:pointer (:struct famconnection)))
  (req (:pointer (:struct famrequest))))

(defcfun ("FAMPending" %fam-pending) :int
  (conn (:pointer (:struct famconnection))))

(defcfun ("FAMNextEvent" %fam-next-event) :int
  (conn (:pointer (:struct famconnection)))
  (fa (:pointer (:struct famevent))))

;;; high level interface

(defclass fam-connection ()
  ((%connection :initarg :connection :type cffi:foreign-pointer)
   ;; Map of request number to request object
   (requests :initform (make-hash-table))
   (fd :initarg :fd :type integer)
   (%open-flag :initform (list t)))
  (:documentation "Stores information about FAM connection. If garbage
collected then C level FAM connection will be automatically
closed"))

(defclass fam-request ()
  ((request-number :initarg :request-number :reader fam-request-number)
   (filename :initarg :filename :reader fam-filename)
   ;; pointer back to our connection
   (connection :initarg :connection :type fam-connection :reader fam-connection)
   (status :initform nil :reader fam-status))
  (:documentation "Base class for FAM request"))

(defclass fam-directory-request (fam-request) ()
  (:documentation "FAM request to monitor a directory"))

(defclass fam-file-request (fam-request) ()
  (:documentation "FAM request to monitor a file"))

(defclass fam-event ()
  ((hostname :initarg :hostname :reader fam-hostname)
   (filename :initarg :filename :reader fam-filename)
   (code :initarg :code :reader fam-code)
   ;; pointers back to FAM-CONNECTION and FAM-REQUEST
   (request :initarg :request :type fam-request :reader fam-request)
   (connection :initarg :connection :type fam-connection :reader fam-connection))
  (:documentation "Contains information about FAM event, is returned by FAM-NEXT-EVENT"))

(defvar *fam* nil
  "Current FAM connection")

(defun fam-open-p (&optional (conn *fam*))
  "Return T if connection is open"
  (when conn (first (slot-value conn '%open-flag))))

(defun check-connection (conn)
  "Throw error unless connection is non-NIL and open"
  (unless (fam-open-p conn)
    (if (null conn) (error "FAM connection is not open, did you forgot to call ~S" 'fam-init)
        (error "FAM connection is closed"))))

(defun fam-open (&optional (app-name "cffi") (reuse t))
  "Optionally open new FAM connection and return it.

With REUSE flag return default connection if its open, or create a new
connection and make it default. Default connection is stored in *FAM*
special variable.

Without REUSE flag, unconditionally create new connection, and return
it without storing it in *FAM*.
"
  (if (and reuse (fam-open-p)) *fam* 
      (let ((%conn (foreign-alloc '(:struct famconnection))))
        (cond ((zerop (%fam-open-2 %conn app-name))
               (let* ((conn (make-instance 'fam-connection :connection %conn
                             :fd (foreign-slot-value %conn '(:struct famconnection) 'fd)))
                      (%open-flag (slot-value conn '%open-flag)))
                 (tg:finalize conn (lambda ()
                                     (when (first %open-flag) 
                                       (%fam-close %conn)
                                       (foreign-free %conn)
                                       (setf (first %open-flag) nil))))
                 (when reuse (setq *fam* conn))
                 conn))
              (t (foreign-free %conn)
                 (error "FAMOpen2 failed with error code ~d" %fam-errno))))))

(defun fam-init (&optional (app-name "cffi"))
  "Open default FAM connection if its not already opened, same as (FAM-OPEN app-name T)"
  (unless (fam-open-p) (fam-open app-name)))

(defun fam-fd (&optional (conn *fam*))
  "Return FAM file handle"
  (check-connection conn)
  (slot-value conn 'fd))

(defun fam-close (&optional (conn *fam*))
  "Close the FAM connection"
  (check-connection conn)
  (with-slots (%connection %open-flag) conn 
    (unwind-protect
         (unless (zerop (%fam-close %connection))
           (error "FAMClose failed with error code ~d" %fam-errno)) 
      (foreign-free %connection)
      (setf (first %open-flag) nil
            %connection nil)))
  (values))

(defun fam-cancel-monitor (req)
  "Cancel monitoring request. "
  (declare (type fam-request req))
  (with-slots (connection request-number status) req
    (check-connection connection)
    (case status
      (:pending-cancel (error "Already requested cancel"))
      (:canceled (error "Already canceled"))
      (t (with-slots (%connection) connection 
           (with-foreign-object (fr '(:struct famrequest))
             (setf (foreign-slot-value fr '(:struct famrequest) 'reqnum)
                   request-number)
             (if (zerop (%fam-cancel-monitor %connection fr))
                 (setf status :pending-cancel)
                 (error "FAMCancelMonitor failed with error code ~d" %fam-errno))))))))

(defun fam-suspend-monitor (req)
  "Suspend monitoring request. "
  (declare (type fam-request req))
  (with-slots (connection request-number) req
    (check-connection connection)
    (with-slots (%connection requests) connection 
      (with-foreign-object (fr '(:struct famrequest))
        (setf (foreign-slot-value fr '(:struct famrequest) 'reqnum)
              request-number)
        (unless (zerop (%fam-suspend-monitor %connection fr))
          (error "FAMSuspendMonitor failed with error code ~d" %fam-errno))))))

(defun fam-resume-monitor (req)
  "Resume monitoring request. "
  (declare (type fam-request req))
  (with-slots (connection request-number) req
    (check-connection connection)
    (with-slots (%connection requests) connection 
      (with-foreign-object (fr '(:struct famrequest))
        (setf (foreign-slot-value fr '(:struct famrequest) 'reqnum)
              request-number)
        (unless (zerop (%fam-resume-monitor %connection fr))
          (error "FAMResumeMonitor failed with error code ~d" %fam-errno))))))

(defun fam-requests (&optional (conn *fam*))
  "Return all the active requests for the connection"
  (check-connection conn)
  (loop for req being each hash-value in (slot-value conn 'requests)
            collect req))

(defun fam-monitor-any (kind filename &optional (conn *fam*) force-dup)
  "Common code for FAM-MONITOR-DIRECTORY and FAM-MONITOR-FILE"
  (declare (type (member :directory :file) kind))
  (check-connection conn)
  (let ((name (merge-pathnames filename)))
    ;; append slash in case it was missing
    (when (and (eq :directory kind)
               (pathname-name name))
      (setq name (make-pathname :directory (append (pathname-directory name)
                                                   (list (pathname-name name))))))
    (setq filename (namestring name)))
  (setq filename (namestring (merge-pathnames filename)))
  (let ((existing-req
          (find-if (lambda (req)
                     (and
                      (case kind
                        (:directory (typep req 'fam-directory-request))
                        (:file (typep req 'fam-file-request)))
                      (equal (fam-filename req) filename)
                      (not (member (fam-status req) '(:canceled :pending-cancel)))))
                   (fam-requests conn))))
    (if (or (null existing-req) force-dup)
        (with-slots (%connection requests) conn 
          (with-foreign-object (fr '(:struct famrequest))
            (cond ((zerop
                    (ecase kind
                      (:directory (%fam-monitor-directory %connection (namestring filename) fr (null-pointer)))
                      (:file (%fam-monitor-file %connection (namestring filename) fr (null-pointer)))))
                   (let* ((reqnum (foreign-slot-value fr '(:struct famrequest) 'reqnum))
                          (req (setf (gethash reqnum requests)
                                     (case kind
                                       (:directory 
                                        (make-instance 'fam-directory-request
                                         :connection conn
                                         :filename filename
                                         :request-number reqnum))
                                       (:file 
                                        (make-instance 'fam-file-request
                                         :connection conn
                                         :filename filename
                                         :request-number reqnum))))))
                     (values req t)))
                  (t (error "~A failed with error code ~d"
                            (case kind (:directory "FAMMonitorDirectory")
                                  (:file "FAMMonitorFile"))
                            %fam-errno)))))
        (values existing-req nil))))

(defun fam-monitor-directory (filename &optional (conn *fam*) force-dup)
  "Monitor directory for changes. Returns FAM-DIRECTORY-REQUEST
object, which can be used to cancel or suspend the request. The same
request object will be returned as a slot of FAM-NEXT-EVENT.

In case the directory is already being monitored, return the existing
request rather then starting a new one, unless FORCE-DUP is also
specified.

The second returned value will be T if new request was created"
  (fam-monitor-any :directory filename conn force-dup))

(defun fam-monitor-file (filename &optional (conn *fam*) force-dup)
  "Monitor file for changes. Returns FAM-FILE-REQUEST object, which
can be used to cancel or suspend the request. The same request object
will be returned as a slot of FAM-NEXT-EVENT.

In case the directory is already being monitored, return the existing
request rather then starting a new one, unless FORCE-DUP is also
specified.

The second returned value will be T if new request was created"
  (fam-monitor-any :file filename conn force-dup))

(defun fam-pending-p (&optional (conn *fam*))
  "Return number of events pending on FAM connection or NIL if none are."
  (check-connection conn)
  (let ((n (%fam-pending (slot-value conn '%connection))))
    (when (plusp n) n)))

(defun translate-fam-code (code)
  "Return FAM keyword for code"
  (declare (type (integer 1 9) code))
  (aref  #(:fam-changed 
           :fam-deleted 
           :fam-startexecuting 
           :fam-stopexecuting 
           :fam-created 
           :fam-moved 
           :fam-acknowledge 
           :fam-exists 
           :fam-endexist)
         (1- code)))

(defun fam-next-event (&optional use-values (conn *fam*))
  "Return queued FAM event from the specified connection, if no events
are pending will wait for one. By default the event is returned as an
instance of FAM-EVENT class.

The FAM-CODE is translated into the keyword, 

With USE-VALUES flag returns event code, filename and FAM-REQUEST
object as three values. 
"
  (check-connection conn)
  (with-slots (%connection requests) conn 
    (with-foreign-object (event '(:struct famevent))
      (if (plusp (%fam-next-event %connection event)) 
          (let* ((reqnum (foreign-slot-value event '(:struct famevent) 'fr))
                 (request (gethash reqnum requests))
                 (hostname (foreign-slot-value event '(:struct famevent) 'hostname))
                 (filename (foreign-string-to-lisp (foreign-slot-pointer event '(:struct famevent) 'filename)))
                 (code (translate-fam-code (foreign-slot-value event '(:struct famevent) 'code))))
            (when (and request (eq code :fam-acknowledge))
              (with-slots (status) request 
                (setf status :canceled)
                (remhash reqnum requests)))
            (if use-values (values code filename request)
                (make-instance 'fam-event
                 :connection conn
                 :request request
                 :code code
                 :filename filename
                 :hostname hostname)))
          (error "FAMNextEvent failed with code ~d" %fam-errno)))))
