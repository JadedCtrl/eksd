(require "asdf")

(asdf:defsystem "eksd"
  :version "0.11"
  :license "GPLv3"
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :homepage "https://hak.xwx.moe/jadedctrl/eksd"
  :description "For reading files into hex— `xxd`-like with text-tables."
  :depends-on ()
  :components ((:file "src/eksd"))
  :in-order-to ((test-op (test-op "eksd/tests"))
                (build-op (build-op "eksd/unix"))))

(asdf:defsystem "eksd/unix"
  :version "0.11"
  :license "GPLv3"
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :homepage "https://hak.xwx.moe/jadedctrl/eksd"
  :description "UNIX terminal front-tend to eksd. `xxd` twin."
  :class asdf:program-system
  :build-operation "program-op"
  :build-pathname "eksd"
  :entry-point "eksd/unix:main"
  :depends-on (:cl-strings :eksd :unix-opts)
  :components ((:file "src/unix"))
  :in-order-to ((test-op (test-op "eksd/unix/tests"))))



;;; Tests
;;; —————————————————————————————————————
(asdf:defsystem "eksd/tests"
  :version "0.11"
  :license "GPLv3"
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :homepage "https://hak.xwx.moe/jadedctrl/eksd"
  :description "Tests for the the eksd package."
  :depends-on (:eksd :lisp-unit2)
  :components ((:file "t/eksd")))

(asdf:defsystem "eksd/unix/tests"
  :version "0.11"
  :license "GPLv3"
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :homepage "https://hak.xwx.moe/jadedctrl/eksd"
  :description "Tests for the eksd.unix package."
  :depends-on (:eksd/unix :lisp-unit2)
  :components ((:file "t/unix")))


;; Following methods borrowed from lisp-unit2’s documentation:
;; https://github.com/AccelerationNet/lisp-unit2/blob/master/README.md#asdf
(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :eksd/tests))))
  (eval (read-from-string
         "(lisp-unit2:with-summary ()
            (lisp-unit2:run-tests :package :eksd/tests))")))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :eksd/unix/tests))))
  (eval (read-from-string
         "(lisp-unit2:with-summary ()
            (lisp-unit2:run-tests :package :eksd/unix/tests))")))
