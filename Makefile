install:
	raco pkg install sicp
	raco pkg install review

test:
	raco test sicp

lint:
	@echo "check codestyle"
	@(for f in $$(find sicp -name '*.rkt'); do raco review $$f; done)

repl:
	racket -i -p neil/sicp -l xrepl

.PHONY: test
