(in-package :cl-fam)
(include "fam.h")

(cstruct famconnection "struct FAMConnection"
         (fd "fd" :type :int)
         (client "client" :type :pointer))

(cstruct famrequest "struct FAMRequest"
         (reqnum "reqnum" :type :int))

(cstruct famevent "FAMEvent"
         (fc "fc" :type :pointer)
         (fr "fr" :type :int)
         (hostname "hostname" :type :string)
         (filename "filename" :type :char)
         (userdata "userdata" :type :pointer)
         (code "code" :type :int))

