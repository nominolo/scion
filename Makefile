default: inplace

TOP := $(shell pwd)
DIST = $(HOME)/tmp/dist-devel/scion-0.4/
HC ?= ghc
RUNHC ?= runghc

#HC = ghc-6.12.1
#RUNHC = runghc -f$(HC)

boot:
	mkdir -p $(DIST)

.PHONY: inplace
inplace:
	$(HC) --make -outputdir $(DIST) -isrc -package ghc Scion.Session
	$(HC) --make -outputdir $(DIST) -isrc -package ghc Scion.Worker.Main
	$(HC) --make -outputdir $(DIST) -isrc -package ghc src/Worker.hs -o $(DIST)/scion-worker
#	cp src/Worker.hs $(DIST)/Worker.hs
	echo "#!/bin/sh\n$(DIST)/scion-worker \$${1+\"\$$@\"}" > inplace/scion-worker
	chmod +x inplace/scion-worker
	echo "#!/bin/sh\n$(RUNHC) -i\"$(TOP)/src\" -package --ghc-arg=ghc -i\"$(DIST)\" \"$(TOP)/src/Server.hs\"" > inplace/scion-server
	chmod +x inplace/scion-server

.PHONY: install
install:
	cabal -v install --builddir=$(DIST)/cabal

.PHONY: test
test:
	runghc test/TestSuite.hs