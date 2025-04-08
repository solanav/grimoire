LISP ?= ros run --

golem:
	rm -rf build/

	$(LISP) --load golem/golem.asd \
		--eval '(asdf:load-system :golem)' \
		--eval '(push :deploy-console *features*)' \
		--eval '(asdf:make :golem)' \
		--eval '(quit)'

.PHONY: golem
