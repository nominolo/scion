
Introduction
============

[Scion][home] is a Haskell library that aims to provide Haskell source
code inspection and transformation functionality as well as various
other features that may be useful for an IDE.

Most of Scion's functionality is based on the GHC API.  Scion tries to
be front-end agnostic; it provides both a Haskell API and servers for
non-Haskell clients such as Emacs (no Vim, volunteers required).

  [home]: http://code.google.com/p/scion-lib/


Installation
============

(For developer builds see section "Hacking" below.)

Scion requires [GHC 6.10.1][ghc] or later.  All other dependencies
should be on [Hackage][hackage] and can be installed using
[cabal-install][ci] in the lib directory:

    $ cd dir/to/scion/lib
    $ cabal install

Scion supports various configuration flags which are useful when
working on Scion itself.

  [ghc]: http://haskell.org/ghc/download.html
  [hackage]: http://hackage.haskell.org/packages/hackage.html
  [ci]: http://hackage.haskell.org/trac/hackage/wiki/CabalInstall



Usage
=====

Since Scion is a library, you should consult the haddock documentation
for how to use it.  However, you may look at the Emacs frontend for
inspiration.

The Emacs frontend is implemented as a Haskell server. The server is a
separate package, scion-server, which depends on the main scion package.

Emacs
-----

Install Scion with Emacs support:

    $ cd dir/to/scion/server
    $ cabal install

You'll end up with a binary called "scion_server".

    $ ~/.cabal/bin/scion_server

Add the following to your emacs configuration (typically "~/.emacs"):

    (add-to-list 'load-path "<scion>/emacs")
    (require 'scion)

    ;; if ./cabal/bin is not in your $PATH
    (setq scion-program "~/.cabal/bin/scion_server")

    (defun my-haskell-hook ()
      ;; Whenever we open a file in Haskell mode, also activate Scion
      (scion-mode 1)
      ;; Whenever a file is saved, immediately type check it and
      ;; highlight errors/warnings in the source.
      (scion-flycheck-on-save 1))

    (add-hook 'haskell-mode-hook 'my-haskell-hook)
    
Scion mode needs to communicate with the external server.  By default
it will automatically start the server when needed.  See "Manually
Connecting to Scion" below for how to connect to the server manually. 

The scion server process inherits the environment variables from the
Emacs process.  Depending on your system this may be different than
what you'd get if you started the server from the shell.  To adjust
the `PATH` environment variable from within Emacs, add something like
the following to your `.emacs`:

    ;; add ~/usr/bin to the PATH
    (setenv "PATH" "$HOME/usr/bin:$PATH" t)

Once you have a running and connected Scion server, you can use the
commands provided by scion-mode:
 
  * `C-c C-x C-l` (`scion-load`) load the current file with Scion.  If
    the file is within a Cabal project this will prompt to use the
    settings from one of the components in the package description
    file.  You can still choose to load only the current file using
    the default settings.

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
    at point.  (Really only makes sense within an `OPTiONS_GHC` pragma.)

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


Bug Reports
===========

Please send bug reports or feature requests to the [Issue tracker][issues].

  [issues]: http://code.google.com/p/scion-lib/issues/list

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

For development it is probably easier to use the GNU make than Cabal
directly.  The makefile includes a file called `config.mk` which is
not present by default.  You can use the provided `config.mk.sample`
and edit it:

    $ cp config.mk.sample config.mk
    $ edit config.mk

After that, the makefile takes care of the rest.

    $ make           # configure and build
    $ make install   # configure, build, and install

If you don't have the dependencies, yet, and have `cabal-install`, the
following may be helpful (If it's not in the path, adjust `config.mk`
accordingly):

    $ make cabal-install

(This also installs Scion, but that shouldn't interfere with hacking.)


Using an in-place GHC
---------------------

GHC 6.10.1 has a couple of problems.  For example, not all error
messages are reported using the GHC API but instead are printed to
stdout/stderr.  Some parts also call `exitWith` directly.  GHC's HEAD
branch has some of these bugs fixed and may contain new features not
present in the stable branch.  If you want to compile against an
inplace GHC, the following steps should work:

 1. On windows, make sure that Cabal finds the inplace gcc

        $ cd /path/to/ghc
        $ cp `which gcc` ghc/

    (Adjust to version of GCC that GHC was compiled with.)

 2. Set the `GHC_PATH` variable to the correct path to for your
    system.  Make sure *not* to set `HC`, `PKG`, or `HADDOCK`, they
    will automatically be set to point to the inplace versions.

 3. Use `make` or `make cabal-install` as above.
