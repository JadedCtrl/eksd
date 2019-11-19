(defsystem "eksd"
	   :version "0.1"
           :license "GPLv3"
	   :author "Jaidyn Ann <jadedctrl@teknik.io>"
	   :description "For reading files into hex— `xxd`-like with text-tables."
	   :depends-on ()
	   :components ((:file "eksd")))

(defsystem "eksd-unix"
	   :version "0.1"
           :license "GPLv3"
	   :author "Jaidyn Ann <jadedctrl@teknik.io>"
	   :description "UNIX terminal front-tend to eksd. `xxd` twin."
	   :depends-on (:eksd :unix-opts :cl-strings)
	   :components ((:file "eksd-unix")))
