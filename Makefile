.PHONY: default clean install-lib install-deps setup test

default: all
all: build

-include config.mk

# Make sure we know the absolute path of the current directory.
TOP     := $(shell pwd)

# If not set in custom config.mk, use the default versions
HC      ?= ghc
PKG     ?= ghc-pkg
HADDOCK ?= haddock
CABAL   ?= cabal
DIST    ?= $(TOP)/dist

DIST_LIB  = $(DIST)/lib
DIST_SERVER = $(DIST)/server
SETUP_DIST = $(DIST)/setup
SETUP = $(SETUP_DIST)/Setup

# Use this directory for testing from-scratch builds. (TODO)
DIST_SCRATCH = $(DIST)/scratch

CABAL_INSTALL_OPTS += --ghc --with-compiler=$(HC) --with-hc-pkg=$(PKG)
CABAL_FLAGS ?= 
# -ftesting

$(DIST)/setup-config: $(SETUP) scion.cabal $(DIST)
	$(SETUP) configure -v --builddir=$(DIST) \
	     --with-compiler=$(HC) --with-hc-pkg=$(PKG) \
             --user $(CABAL_FLAGS) > $(DIST)/lib-config-log

$(DIST)/build/libHSscion-0.1.a: $(SETUP) $(DIST)/setup-config $(wildcard lib/**/*.hs lib/**/**/*.hs server/**/*.hs)
	@echo === Building scion ===
	$(SETUP) build --builddir=$(DIST)

$(DIST):
	mkdir -p $@

$(SETUP): Setup.hs $(SETUP_DIST)
	$(HC) --make $< -o $@

$(SETUP_DIST):
	mkdir -p $@

setup: $(SETUP)

build: $(DIST)/build/libHSscion-0.1.a

# TODO: dodgy
install: $(DIST)/build/libHSscion-0.1.a
	$(CABAL) install

# test: build
# 	echo main | $(HC) --interactive -package ghc -DDEBUG -isrc -idist/build tests/RunTests.hs
# #	./dist/build/test_get_imports/test_get_imports $(GHC_PATH)/compiler dist-stage2 +RTS -s -RTS

clean:
	$(SETUP) clean --builddir=$(DIST) || rm -rf $(DIST)

distclean: clean
	rm -rf $(SETUP_DIST)

# doc: configure
# 	$(SETUP) haddock --with-haddock=$(HADDOCK)

printvars:
	@echo "UseInplaceGhc      = $(UseInplaceGhc)"
	@echo "GHC_PATH           = $(GHC_PATH)"
	@echo "HC                 = $(HC)"
	@echo "PKG                = $(PKG)"
	@echo "HADDOCK            = $(HADDOCK)"
	@echo "CABAL              = $(CABAL_INSTALL)"
	@echo "CABAL_INSTALL_OPTS = $(CABAL_INSTALL_OPTS)"
	@echo "CABAL_FLAGS        = $(CABAL_FLAGS)"
	@echo "DIST               = $(DIST)"
	@echo "TOP                = $(TOP)"
