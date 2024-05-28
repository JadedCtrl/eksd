LISP ?= sbcl

all: clean build

clean:
	-rm eksd

quicklisp:
	curl "https://beta.quicklisp.org/quicklisp.lisp" -o /tmp/ql.lisp
	$(LISP) --load /tmp/ql.lisp \
	--eval '(quicklisp-quickstart:install :path "~/.local/lib/quicklisp/")' \
	--eval '(ql:add-to-init-file)' \
	--eval '(quit)'

build:
	$(LISP) --load eksd.asd \
	--eval '(ql:quickload :eksd)' \
	--eval '(ql:quickload :eksd/unix)' \
	--eval '(asdf:make :eksd/unix)' \
	--eval '(quit)'

test:
	$(LISP) --load eksd.asd \
	--eval '(ql:quickload :eksd)' \
	--eval '(ql:quickload :eksd/unix)' \
	--eval '(asdf:test-system :eksd)' \
	--eval '(asdf:test-system :eksd/unix)' \
	--eval '(quit)'
