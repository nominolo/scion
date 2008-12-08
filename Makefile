include config.mk

# If not set in custom config.mk, use the inplace GHC
HC      ?= $(GHC_PATH)/ghc/stage2-inplace/ghc
PKG     ?= $(GHC_PATH)/utils/ghc-pkg/install-inplace/bin/ghc-pkg
HADDOCK ?= $(GHC_PATH)/utils/haddock/install-inplace/bin/haddock

DIST  = dist
SETUP_DIST = setup-dist
SETUP_CONFIG = $(DIST)/setup-config
SETUP = $(SETUP_DIST)/Setup

main: build

setup: $(SETUP)
$(SETUP): Setup.hs
	@mkdir -p $(SETUP_DIST)
	$(HC) --make -odir $(SETUP_DIST) -hidir $(SETUP_DIST) -o $@ $<

configure: $(SETUP_CONFIG)
$(SETUP_CONFIG): scion.cabal setup
	$(SETUP) configure -v --with-compiler=$(HC) --with-hc-pkg=$(PKG) --user -ftesting -femacs

.PHONY: build
build: configure
	$(SETUP) build

install: build
	$(SETUP) install

test: build
	echo main | $(HC) --interactive -package ghc -DDEBUG -isrc -idist/build tests/RunTests.hs
#	./dist/build/test_get_imports/test_get_imports $(GHC_PATH)/compiler dist-stage2 +RTS -s -RTS

clean:
	$(SETUP) clean

distclean: clean
	rm -rf $(SETUP_DIST)

doc:
	$(SETUP) haddock --with-haddock=$(HADDOCK)

printvars:
	@echo "GHC_PATH = $(GHC_PATH)"
	@echo "HC       = $(HC)"
	@echo "PKG      = $(PKG)"
	@echo "HADDOCK  = $(HADDOCK)"