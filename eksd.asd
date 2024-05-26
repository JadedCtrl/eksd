(defsystem "eksd"
  :version "0.11"
  :license "GPLv3"
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :homepage "https://hak.xwx.moe/jadedctrl/eksd"
  :description "For reading files into hex— `xxd`-like with text-tables."
  :depends-on ()
  :components ((:file "src/eksd")))

(defsystem "eksd.unix"
  :version "0.11"
  :license "GPLv3"
  :author "Jaidyn Ann <jadedctrl@posteo.at>"
  :homepage "https://hak.xwx.moe/jadedctrl/eksd"
  :description "UNIX terminal front-tend to eksd. `xxd` twin."
  :depends-on (:eksd :unix-opts :cl-strings)
  :components ((:file "src/unix")))
