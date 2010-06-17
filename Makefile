.PHONY: default all
default: all
all: build

-include config.mk

# Make sure we know the absolute path of the current directory.
TOP     := $(shell pwd)
VERSION = 0.2.0.1

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

SERVER_DEPS := $(shell find ./server -name '*.hs')
LIB_DEPS := $(shell find ./lib -name '*.hs')


$(DIST)/setup-config: scion.cabal
	$(CABAL) configure -v --builddir=$(DIST) \
	     --with-compiler=$(HC) --with-hc-pkg=$(PKG) \
	     -f-server \
             --user $(CABAL_FLAGS) > $(DIST)/lib-config-log

$(DIST)/build/libHSscion-$(VERSION).a: $(DIST)/setup-config $(LIB_DEPS)
	@echo === Building scion ===
	$(CABAL) build --builddir=$(DIST)

TEST_DB = $(DIST)/manual/testdb.conf
$(TEST_DB):
	$(PKG) init $@

$(DIST)/manual:
	mkdir -p $@

# TODO: dodgy
.PHONY: install
install: $(DIST)/manual/.installed
$(DIST)/manual/.installed: $(DIST)/build/libHSscion-$(VERSION).a $(TEST_DB)
	$(CABAL) install --builddir=$(DIST) --package-db=$(TEST_DB)
	@touch $@

$(DIST):
	mkdir -p $@

$(SETUP): Setup.hs $(SETUP_DIST)
	$(HC) --make $< -o $@

$(SETUP_DIST):
	mkdir -p $@

.PHONY: setup
setup: $(SETUP)

.PHONY: build
build: $(DIST)/build/libHSscion-$(VERSION).a

.PHONY: rebuild
rebuild: clean build

$(DIST)/manual/dist-worker:
	mkdir -p $@

$(DIST)/manual/dist-bench:
	mkdir -p $@

$(DIST)/manual/dist-server:
	mkdir -p $@

.PHONY: printdeps
printdeps:
	@echo "SERVER_DEPS = $(SERVER_DEPS)"
	@echo "LIB_DEPS = $(LIB_DEPS)"

$(DIST)/manual/scion-worker: server/Worker.hs $(SERVER_DEPS) $(DIST)/manual/.installed
	mkdir -p $(DIST)/manual/dist-worker
	$(HC) --make -odir $(DIST)/manual/dist-worker -package ghc -iserver -package-conf $(TEST_DB) -o $@ $<

$(DIST)/manual/bench: bench/ClientServer.hs $(SERVER_DEPS) $(DIST)/manual/.installed
	mkdir -p $(DIST)/manual/dist-bench
	$(HC) --make -odir $(DIST)/manual/dist-bench -iserver -package-conf $(TEST_DB) -o $@ $<

$(DIST)/manual/scion-server: server/Proxy.hs $(SERVER_DEPS) $(DIST)/manual/.installed
	mkdir -p $(DIST)/manual/dist-server 
	$(HC) --make -odir $(DIST)/manual/dist-server -package ghc -iserver -package-conf $(TEST_DB) -o $@ $<

.PHONY: worker server
worker: $(DIST)/manual/scion-worker
server: $(DIST)/manual/scion-server $(DIST)/manual/scion-worker

.PHONY: bench
bench: $(DIST)/manual/bench $(DIST)/manual/worker
	$(DIST)/manual/bench

# test: build
# 	echo main | $(HC) --interactive -package ghc -DDEBUG -isrc -idist/build tests/RunTests.hs
# #	./dist/build/test_get_imports/test_get_imports $(GHC_PATH)/compiler dist-stage2 +RTS -s -RTS

.PHONY: clean distclean printvars
clean:
	$(SETUP) clean --builddir=$(DIST) || rm -rf $(DIST)
	rm -rf $(DIST)/manual

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
