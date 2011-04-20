Since version 0.3 Scion uses a multi-process architecture.  The Scion
library starts one or more `scion-worker` processes which do the
actual work.  The Scion library just manages these processes (and
caches some of their state).  This solves the following problems:

  - *Static Flags*.  Some of GHC's command line flags can only be set
    on start-up.  This is important mainly for flags that control the
    kind of compilation (profiled, threaded).
    
  - *Other write-once state*.  GHC only reads the package database once
    on startup.  If new packages have been installed since startup
    they will not be visible.  Changing the database by force while a
    session is running is likely to cause problems.
    
  - *Caches*.  There are a few caches in GHC that cannot be flushed.
    These include the name cache, and the package DB cache.
    
  - *Multiple Compiler Versions*.  It is not possible to link to two
    different versions of GHC from within the same program.  If we
    want to make sure a program compiles with multiple versions of GHC
    (or multiple combinations of its dependencies) we need to use
    multiple processes.

The downside of a multi-process architecture is of course the
additional context switches and communication overhead.  To reduce
this, we:

  - use a binary protocol,

  - cache some information on the library side, and

  - avoid sending too much data between library and worker.
  
Non-Haskell front-ends use a scion-server that takes the place of the
library.

The architecture therefore looks as follows:

            +-----------------------+
            |  Non-Haskell frontend |
            | (Eclipse, Emacs, Vim) |
            +-----------------------+
                        ^
                        |  front-end specific protocol
                        |     (e.g., json, s-exprs)
                        v
               +-----------------+
               |  Scion server / |
               |  Scion library  |
               +-----------------+
                 ^      ^      ^
                 |      |      |    binary protocol
                 v      v      v
    +--------------+         +--------------+
    | Scion worker |   ...   | Scion worker |
    +--------------+         +--------------+

If the front-end is written in Haskell, it will take the part of the
Scion library.  The Scion server, in turn, translates between a
front-end-specific serialisation format to Scion library API calls.

The library-worker protocol is defined in `src/Scion/Types/Commands`.
