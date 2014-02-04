This document contains a few notes/ideas/brain dumps for how Scion
works or should work.

# Links

  - [GHC API Haddock documentation][ghc-api]
  - [Hoogle][] for everything else

 [ghc-api]: http://www.haskell.org/ghc/docs/latest/html/libraries/ghc/index.html
 [Hoogle]: http://www.haskell.org/hoogle/

# Session State

## GHC Session State

 * **Static Flags**:  We cannot change these throughout a session.

 * **Dynamic Flags**: May be set on a per-file basis and a per-project
   basis.  They contain all sorts of stuff including search paths and
   more.  The tricky bit here is resetting the old DynFlags when
   loading a new component.

 * **Targets and module graph**: The targets are all the files that
   we are "interested" in.  The module graph contains the targets and
   all the modules it depends on (within the same package).  GHC will
   automatically perform dependency analysis for use (including
   running the C preprocessor.)

 * **Interactive Context**: The context (set of visible modules and
   debugger state) of the byte code interpreter.

## Scion Session State

What we really need is some kind of abstraction of the GHC session
state.  This could be called a "project".  Currently we have two ways
of setting up the information that makes up the GHC session state.

  * We can load just a single file (like GHCi's `load`).

    Idea: Currently we set the working dir to the directory that the
    file is in.  Since, by default, the search path includes only the
    current directory, it might be worthwhile to set up a proper
    search path based on the name of the module.  For example, if we
    load file `/foo/bar/A/B/C.hs` and the module name at the top of
    the file is `A.B.C` then a reasonable choice of search path would
    be `foo/bar`.  (Unfortunately, getting that module name is not
    very easy at the moment.)

  * We can load a component of Cabal file.  Such a component is either
    the library or one of the executables specified within the Cabal
    file.  The difficulty here is that in order to get the proper
    meta-data we have to first configure the project.  When selecting
    such a component the user therefore can do several things:

     1. If the project has not been configured, yet, it has to be
        **configured**.

     2. If the project has been configured previously (either in the
        same session or some other time) the user may want to
        **reconfigure** or **reuse** the existing configuration.

   The first case is obvious.  The second case may be triggered both
   when switching components within the same Cabal project, or when
   re-opening a project that we have been working on previously.

### Configuring a Cabal project

In addition to the Cabal file to configure, we also need

  * A `dist` directory:  The default is `.dist-scion` in the same
    directory that the Cabal file resides.  The main problem with this
    default is that this directory should be ignored by the version
    control system, and therefore requires an action on the part of the
    user.  On the other hand, most VCSs support a global settings file,
    so anyone using Scion only needs to edit the global file once.

  * Command line flags to the `configure` command.

In the future we could provide a mechanism to put this information in
a special file, so the user does not have to provide this information
over and over again.
