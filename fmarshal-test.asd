(asdf:defsystem fmarshal-test
  :version "0"
  :description "Test marshal library"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :licence "BSD-style"
  :depends-on (fiveam fmarshal)
  :serial t
  ;; components likely need manual reordering
  :components ((:file "fmarshal-test")))
