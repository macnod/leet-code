(asdf:defsystem :leet-code
  :description "Leetcode exercises"
  :author "Donnie Cameron <macnod@gmail.com>"
  :license "MIT License"
  :depends-on (:dc-utilities :cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "leet-code")))
