
Introduction
============

[Scion][home] is a Haskell library that aims to implement those parts
of a Haskell IDE which are independent of the particular front-end.
Scion is based on the GHC API and Cabal.  It provides both a Haskell
API and a server for non-Haskell clients such as Emacs and Vim.

  [home]: http://code.google.com/p/scion-lib/


Installation
============

(For developer builds see section "Hacking" below.)

Scion requires [GHC 6.10.1][ghc] or later.  All other dependencies
should be on [Hackage][hackage] and can be installed using
[cabal-install][ci].  Scion consists of a library and a server which
is used by front-ends that are not written in Haskell.

To install the library and server use:

    $ cd dir/to/scion
    $ cabal install
   
This will install the executable `scion_server` in the `bin` directory
of `cabal-install`, typically `$HOME/.cabal/bin`.

If you do not want to install the server (and its dependencies), turn
off the "server" flag which is enabled by default:

    $ cabal install -f-server

In order to use scion with your favourite front-end, see the specific
instructions for the front-end below.  The Emacs and Vim front-ends
are included with Scion and their installation instruction follow
below.  The necessary files are installed with Scion by default and
there is currently no option to turn this off.

  [ghc]: http://haskell.org/ghc/download.html
  [hackage]: http://hackage.haskell.org/packages/hackage.html
  [ci]: http://hackage.haskell.org/trac/hackage/wiki/CabalInstall


Bug Reports
===========

Please send bug reports or feature requests to the [Issue tracker][issues].

  [issues]: http://code.google.com/p/scion-lib/issues/list


Usage
=====

Since Scion is a library, you should consult the haddock documentation
for how to use it.  However, you may look at the Emacs frontend for
inspiration.

Emacs
=====

Scion's Emacs mode should be seen as complimentary to the existing
Haskell mode.  To use it install the Scion server as described
above. In the following we'll assume that the server has been install
as:

    $ ~/.cabal/bin/scion-server

Add the following to your emacs configuration (typically "~/.emacs"):

    ;; Substitute the desired version for <version>
    (add-to-list 'load-path "~/.cabal/share/scion-<version>/emacs")
    (require 'scion)

    ;; if ./cabal/bin is not in your $PATH
    (setq scion-program "~/.cabal/bin/scion-server")

    (defun my-haskell-hook ()
      ;; Whenever we open a file in Haskell mode, also activate Scion
      (scion-mode 1)
      ;; Whenever a file is saved, immediately type check it and
      ;; highlight errors/warnings in the source.
      (scion-flycheck-on-save 1))

    (add-hook 'haskell-mode-hook 'my-haskell-hook)

    ;; Use ido-mode completion (matches anywhere, not just beginning)
    ;;
    ;; WARNING: This causes some versions of Emacs to fail so badly
    ;; that Emacs needs to be restarted.
    (setq scion-completing-read-function 'ido-completing-read)

Scion mode needs to communicate with the external server.  By default
the server will be started automatically when needed.  See "Manually
Connecting to Scion" below for how to connect to the server manually.

Scion uses Cabal as a library which in turn might look for external
programs such as [happy][] or [alex][].  In order to find these, the
`PATH` environment variable has to be set up correctly.

  [happy]: http://www.haskell.org/happy/
  [alex]: http://www.haskell.org/alex/

The scion server process inherits the environment variables from the
Emacs process.  Depending on your system this may be different than
what you would get if you started the server from the shell.  To
adjust the `PATH` environment variable from within Emacs, add
something like the following to your `.emacs`:

    ;; add ~/usr/bin to the PATH
    (setenv "PATH" "$HOME/usr/bin:$PATH" t)

Once you have a running and connected Scion server, you can use the
commands provided by scion-mode:
 
  * `C-c C-x C-l` (`scion-load`) load the current file with Scion.  If
    the file is within a Cabal project this will prompt to use the
    settings from one of the components in the package description
    file.  You can still choose to load only the current file.

  * `C-c C-o` (`scion-open-cabal-project`) configures a Cabal project
    and loads the meta-data from a Cabal file.  Note that this
    does not type check or load anything.  If you change the Cabal
    file of a project, call this function to update the session with
    the new settings.

If loading generates any errors or warnings, a buffer will appear and
list them all.  Pressing `RET` on a note will jump to its source
location.  Pressing `q` closes the buffer, and `C-c C-n`
(`scion-list-compiler-notes`) brings it back.  Use `M-n`
(`scion-next-note-in-buffer`) and `M-p`
(`scion-previous-note-in-buffer`) to navigate within the notes of one
buffer.

## Completion

The following commands offer completion for a few things.

  * `C-c i l` (`haskell-insert-language`) asks for a `LANGUAGE` pragma
    and adds it to the top of the file.
  
  * `C-c i p` (`haskell-insert-pragma`) inserts a pragma at the
    current cursor position.  (At the moment this doesn't try to make
    sense of the selected pragma, however.)
    
  * `C-c i m` (`haskell-insert-module-name`) inserts the name of an
    external module (external), i.e., a module _not_ from the current
    package.
    
  * `C-c i f` (`haskell-insert-flag`) insert (GHC) command line flag
    at point.  (Really only makes sense within an `OPTIONS_GHC` pragma.)

## Experimental features

The following should work most of the cases.

  * `C-c C-.` (`scion-goto-definition`) jumps to the definition of the
    identifier at point.  If there is no identifier at point, offers a
    list to complete on a particular identifier.  This currently only
    works for identifiers defined within the same project.

  * `C-c C-t` shows type of identifier at point.  This only works if
    the current file typechecks, but then it also works for local
    identifiers.  For polymorphic function it will show the type to
    which they are _instantiated_, e.g.,

        f x = x + (1::Int)

    Calling this command on `+` will print `Int -> Int -> Int` instead
    of `Num a => a -> a -> a`.

    
# Manually Connecting to Scion

If you set the variable `scion-auto-connect` to `'ask` (the default is
`'always`), Scion will ask whether to start the server.  If you set it
to `nil` you need to manually connect to the server.

You can start the server manually on the command line and then use

    M-x scion-connect

to connect to that server.  However, most of the time it will be more
convenient to start the server from within Emacs:  

    M-x scion


Vim
===

Vim backend code is hosted on http://github.com/MarcWeber/scion-backend-vim

Discussion
==========

For discussions about Scion use the [scion-lib-devel][ml] mailing list.

  [ml]: http://groups.google.com/group/scion-lib-devel


Hacking
=======

The main repository for Scion is hosted on [Github][gh].  Get it via

    $ git clone git://github.com/nominolo/scion

Send patches or pull requests to nominolo (email address at googlemail
dot com).  Note that, if you fork the project on Github your fork
won't take up additional space on your account.

  [gh]: http://github.com


Building
--------

For development it is probably easier to use the GNU Make than Cabal
directly.  The makefile includes a file called `config.mk` which is
not present by default.  You can use the provided `config.mk.sample`
and edit it:

    $ cp config.mk.sample config.mk
    $ edit config.mk

After that, the makefile takes care of the rest.

    $ make           # configure and build
    $ make install   # configure, build, and install


Using an in-place GHC
---------------------

GHC 6.10.1 has a couple of problems.  For example, not all error
messages are reported using the GHC API but instead are printed to
stdout/stderr.  Some parts also call `exitWith` directly.  GHC's HEAD
branch has some of these bugs fixed and may contain new features not
present in the stable branch.  If you want to compile against an
inplace GHC, the following steps should work:

 1. On Windows, make sure that Cabal finds the inplace gcc

        $ cd /path/to/ghc
        $ cp `which gcc` ghc/

    (Adjust to version of GCC that GHC was compiled with.)

 2. Set the `GHC_PATH` variable to the correct path to for your
    system.  Make sure *not* to set `HC`, `PKG`, or `HADDOCK`, they
    will automatically be set to point to the inplace versions.

 3. Use `make`.


License
=======

The parts of Scion written in Haskell are licensed under the BSD
license.  The Emacs lisp parts are licensed under the GPL license
version 2 or (at your option) any later version.


Known Pitfalls
==============
If you get an error message like this:
  "scion_server: mkTopLevEnv: not interpreted main:Main"
then you should rm [Ss]etup.hi [Ss]etup.o in the project directory.
