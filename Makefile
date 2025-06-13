MAKE=@make
DUNE=@dune
LN=@ln -sf
RM=@rm
EXE=analyzer
BISECT=@bisect-ppx-report
ENV=@env
CHECK=@checkml
SRC=${wildcard src/*.ml}

all:
	$(DUNE) build src/main.exe
	$(LN) _build/default/src/main.exe $(EXE)

release:
	$(DUNE) build --profile=release src/main.exe
	$(LN) _build/default/src/main.exe $(EXE)
	@$(foreach item,$(SRC),checkml $(item) || exit $$?;)

fmt:
	$(DUNE) build @fmt --auto-promote

check: release
	$(DUNE) build @fmt

test: release
	$(DUNE) build @fmt
	$(DUNE) test

coverage:
	$(MAKE) clean
	$(ENV) BISECT_SIGTERM=YES \
		timeout --signal=SIGTERM 60s \
		dune runtest --instrument-with bisect_ppx --force || true
	$(BISECT) html
	$(BISECT) summary

clean:
	$(DUNE) clean
	$(RM) -rf $(EXE)
	$(RM) -rf _coverage
