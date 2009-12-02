Using Scion as a Library
========================

Let's start off with a simple example.  The following code loads a
single Haskell file, type checks it, and prints the
number of messages.

    import Scion
    import Scion.Types

    import qualified Data.MultiSet as MS
    
    main = runScion $ do
      r <- loadComponent (Component (FileComp "/path/to/file.hs"))
      io $ print $ MS.size (compilationNotes r)

If instead you want to load a Cabal file, import `Scion.Cabal` and use
the following statements for loading the library or executable
components respectively.

    loadComponent (Component (Library "/path/to/file.cabal"))

    loadComponent (Component (Executable "/path/to/file.cabal"
                                         "executable-name"))

Loading a Cabal component automatically (re-)configures the Cabal
project if necessary, pre-processes non-Haskell files (e.g., Alex or
Happy files), and sets up the search paths.  Note, however, that no
dependecies of the preprocessed files are tracked, that is, if a
preprocessed file is modified you need to manually re-generate the
preprocessed files.  This can be done by calling
`Scion.Cabal.preprocessPackage`.

If you don't know which components are specified in a `.cabal` file,
use `Scion.Cabal.cabalProjectComponents` to list them all.

[XXX: Setting Cabal's parameters is currently not possible.]


Using the GHC API
-----------------

After Scion has loaded a component, all the usual GHC API functions
are available.
