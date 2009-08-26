.PHONY: default clean install-lib install-deps setup

default: all
all: build

include config.mk

# If not set in custom config.mk, use the default versions
HC      ?= ghc
PKG     ?= ghc-pkg
HADDOCK ?= haddock

DIST = dist
DIST_LIB  = $(DIST)/lib
DIST_SERVER = $(DIST)/server
SETUP_DIST = setup-dist
SETUP = $(SETUP_DIST)/Setup

DOTDOTSETUP = cabal

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
	mkdir $(DIST)

$(SETUP): Setup.hs $(SETUP_DIST)
	$(HC) --make $< -o $@

$(SETUP_DIST):
	mkdir $@

setup: $(SETUP)

build: $(DIST)/build/libHSscion-0.1.a

# TODO: dodgy
install: $(DIST)/build/libHSscion-0.1.a
	cabal install

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
	@echo "UseInplaceGhc    = $(UseInplaceGhc)"
	@echo "GHC_PATH         = $(GHC_PATH)"
	@echo "HC               = $(HC)"
	@echo "PKG              = $(PKG)"
	@echo "HADDOCK          = $(HADDOCK)"
	@echo "CABAL_INSTALL    = $(CABAL_INSTALL)"
	@echo "        ..._OPTS = $(CABAL_INSTALL_OPTS)"
	@echo "CABAL_FLAGS      = $(CABAL_FLAGS)"
	@echo "---------------------------------------------------------------"
	@echo "DIST_LIB     = $(DIST_LIB)"
	@echo "SETUP_DIST   = $(SETUP_DIST)"
