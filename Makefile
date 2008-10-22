include config.mk
HC=$(GHC_PATH)/ghc/stage2-inplace/ghc
PKG=$(GHC_PATH)/utils/ghc-pkg/install-inplace/bin/ghc-pkg
SETUP=./Setup

main: build

$(SETUP): Setup.hs
	$(HC) --make $<

configure:
	$(SETUP) configure --with-compiler=$(HC) --with-hc-pkg=$(PKG) --user -ftesting

build:	
	$(SETUP) build

install:
	$(SETUP) install

test:
	echo main | $(HC) --interactive -package ghc -DDEBUG -isrc -idist/build tests/RunTests.hs
#	./dist/build/test_get_imports/test_get_imports $(GHC_PATH)/compiler dist-stage2 +RTS -s -RTS

clean:
	$(SETUP) clean

