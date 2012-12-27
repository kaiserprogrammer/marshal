(asdf:defsystem marshal-test
  :version "0"
  :description "Test marshal library"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :licence "BSD-style"
  :depends-on (fiveam marshal)
  :serial t
  ;; components likely need manual reordering
  :components ((:file "marshal-test")))
