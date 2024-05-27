(require "asdf")

(asdf:defsystem "eksd"
  :version "0.11"
  :license "GPLv3"
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :homepage "https://hak.xwx.moe/jadedctrl/eksd"
  :description "For reading files into hex— `xxd`-like with text-tables."
  :depends-on ()
  :components ((:file "src/eksd")))

(asdf:defsystem "eksd.unix"
  :version "0.11"
  :license "GPLv3"
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :homepage "https://hak.xwx.moe/jadedctrl/eksd"
  :description "UNIX terminal front-tend to eksd. `xxd` twin."
  :class asdf:program-system
  :build-operation "program-op"
  :build-pathname "eksd"
  :entry-point "eksd.unix:main"
  :depends-on (:cl-strings :eksd :unix-opts)
  :components ((:file "src/unix")))
