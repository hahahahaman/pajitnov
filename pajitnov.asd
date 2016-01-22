;;;; pajitnov.asd

(asdf:defsystem #:pajitnov
  :description "Describe pajitnov here"
  :author "Ed Ye <hahahadude@gmail.com>"
  :license "Licenceless Rider"
  :depends-on (#:err)
  :serial t
  :components ((:module src
                :components ((:file "package")
                             (:file "globals")
                             (:file "utils")
                             (:file "block")
                             ;; (:file "init")
                             ;; (:file "input")
                             ;; (:file "render")
                             ;; (:file "update")
                             ;; (:file "cleanup")
                             (:file "pajitnov")))))
