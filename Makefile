default: install

# Create a file config.mk to customise some of these options.  This
# can be useful for example to compile with the HEAD branch of GHC.
# See config.mk.sample for an example.
-include config.mk

TOP := $(shell pwd)
DIST ?= dist
HC ?= ghc
RUNHC ?= runghc -f $(HC)

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
	time cabal -v install --builddir=$(DIST)/cabal --with-compiler=$(HC)

.PHONY: test
test:
	$(RUNHC) test/TestSuite.hs

.PHONY: docs
docs:
	cabal -v haddock --builddir=$(DIST)/cabal
