
#LINUX DEPENDENCIES

  Requires either fam or gamin, and corresponding development package
  (for /usr/include/fam.h). It seem that gamin is a better choice,
  because it uses inotify facility in modern kernels. 

#SYNOPSIS

```common-lisp
(ql:quickload :cl-fam)

(cl-fam:fam-open)
(cl-fam:fam-monitor-directory "/tmp")
(loop while (cl-fam:fam-pending-p) collect (fam-next-event t))
```

For more full description description of the API see http://www.docunext.com/wiki/Gamin

See docstrings for exported functions for more info.

If you want to try it out with multiple threads, bind `cl-fam:*FAM*`
in the thread, this way the FAM connection would be open per-thread.
  

