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

CABAL_INSTALL_OPTS += --ghc --with-compiler=$(HC) --with-hc-pkg=$(PKG)
CABAL_FLAGS ?= 
# -ftesting

$(DIST):
	mkdir $(DIST)

$(DIST_LIB)/setup-config: $(SETUP) lib/scion.cabal $(DIST)
	@echo === Configuring scion ===
	@echo Configure log: $(DIST)/lib-config-log
	@cd lib && \
        (../$(SETUP) configure -v --builddir=../$(DIST_LIB) \
	                      --with-compiler=$(HC) --with-hc-pkg=$(PKG) \
	                      --user $(CABAL_FLAGS)> ../$(DIST)/lib-config-log)

$(DIST_SERVER)/setup-config: $(SETUP) server/scion-server.cabal $(DIST) $(DIST_LIB)/.installed_tag
	@echo === Configuring scion-server ===
	@echo Configure log: $(DIST)/server-config-log
	@cd server && \
	(../$(SETUP) configure -v --builddir=../$(DIST_SERVER) \
	                      --with-compiler=$(HC) --with-hc-pkg=$(PKG) \
	                      --user $(CABAL_FLAGS) > ../$(DIST)/server-config-log)

$(DIST_LIB)/build/libHSscion-0.1.a: $(SETUP) $(DIST_LIB)/setup-config $(lib/**/*.hs wildcard lib/**/**/*.hs)
	@echo === Building scion ===
	@cd lib && \
        ../$(SETUP) build --builddir=../$(DIST_LIB)

$(DIST_LIB)/.installed_tag: $(DIST_LIB)/build/libHSscion-0.1.a $(SETUP)
	@echo === Installing scion ===
	@cd lib && ../$(SETUP) install --user --builddir=../$(DIST_LIB)
	@touch $@

$(DIST_SERVER)/build/scion_server/scion_server: $(SETUP) $(DIST_SERVER)/setup-config server/Main.hs $(wildcard server/Scion/Server/*.hs server/Scion/Server/**/*.hs)
	@echo === Building scion-server ===
	@cd server && \
        ../$(SETUP) build --builddir=../$(DIST_SERVER)

$(SETUP): Setup.hs
	@echo === Building Setup ===
	@mkdir -p $(SETUP_DIST)
	@$(HC) --make -odir $(SETUP_DIST) -hidir $(SETUP_DIST) -o $@ $<

setup: $(SETUP)

build: $(DIST_LIB)/build/libHSscion-0.1.a $(DIST_SERVER)/build/scion_server/scion_server

# test: build
# 	echo main | $(HC) --interactive -package ghc -DDEBUG -isrc -idist/build tests/RunTests.hs
# #	./dist/build/test_get_imports/test_get_imports $(GHC_PATH)/compiler dist-stage2 +RTS -s -RTS

clean:
	@(cd lib && ../$(SETUP) clean --builddir=../$(DIST_LIB)) || rm -rf $(DIST_LIB)
	@(cd server && ../$(SETUP) clean --builddir=../$(DIST_SERVER)) || rm -rf $(DIST_SERVER)

# distclean: clean
# 	rm -rf $(SETUP_DIST)

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

install-deps:
	cabal install --with-compiler=$(HC) --with-hc-pkg=$(PKG) ghc-paths
	cabal install --with-compiler=$(HC) --with-hc-pkg=$(PKG) ghc-syb
	cabal install --with-compiler=$(HC) --with-hc-pkg=$(PKG) multiset
	cabal install --with-compiler=$(HC) --with-hc-pkg=$(PKG) time
	cabal install --with-compiler=$(HC) --with-hc-pkg=$(PKG) uniplate


# cabal-install:
# 	$(CABAL_INSTALL) install $(CABAL_INSTALL_OPTS) $(CABAL_FLAGS)

# run-emacs: build
# 	./$(DIST)/build/scion_emacs/scion_emacs
