
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
[cabal-install][ci]:

    $ cd dir/to/scion
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

The Emacs frontend is implemented as a Haskell server 

Emacs
-----

Install Scion with Emacs support, either via

    $ cabal install scion -femacs

or, if you have a locally copy of Scion

    $ cd <scion>
    $ cabal install -femacs

You'll end up with a binary called "emacs-server".

    $ ./.cabal/bin/emacs_server

Add the following to your emacs configuration (typically "~/.emacs"):

    (add-to-list 'load-path "<scion>/emacs")
    (require 'scion)

    ;; if ./cabal/bin is not in your $PATH
    (setq scion-program "~/.cabal/bin/emacs_server")

    (defun my-haskell-hook ()
      ;; Whenever we open a file in Haskell mode, also activate Scion
      (scion-mode 1)
      ;; Whenever a file is saved, immediately type check it and
      ;; highlight errors/warnings in the source.
      (scion-flycheck-on-save 1))

    (add-hook 'haskell-mode-hook 'my-haskell-hook)

Scion mode needs to communicate with the external server.  You can
start the server manually on the command line and then use

    M-x scion-connect

to connect to that server.  However, most of the time it will be more
convenient to start the server from within Emacs:  

    M-x scion

You might encounter problems with $PATH inheritance, though.  This is
a bug--we're working on it.

Once you have a running and connected Scion server, you can use the
commands provided by scion-mode:
 
  * `M-x scion-open-cabal-project` (`C-c C-o`) configures a .cabal
    project and loads the meta-data from a .cabal file.  Note that
    this doesn't type check or load anything.  If you change the
    .cabal file of a project, call this function to update the session
    with the new settings.

  * `M-x scion-load-library` (`C-c C-l`) type checks all the files in
    the library.

There are a few more utilities:

    C-c i l  -- insert language pragma
    C-c i p  -- insert pragma
    C-c i m  -- insert (external) module name

Some experimental features:

    C-c C-t  -- show type of identifier at point

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
