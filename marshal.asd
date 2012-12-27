(asdf:defsystem marshal
  :version "0"
  :description "dump and load objects"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :licence "LGPL"
  :depends-on (closer-mop)
  :serial t
  ;; components likely need manual reordering
  :components ((:static-file "README.org" :pathname "README.org")
               (:file "marshal")))
