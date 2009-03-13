test: Main.hs
	@ghc --make Main.hs -o $@

run: test
	./test > run.stdout
	@diff -u run.stdout expected.stdout

.PHONY: clean

clean:
	@rm -rf *.o *.hi test run.stdout
