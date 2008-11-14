
Introduction
============

[Scion][home] is a Haskell library that aims to provide Haskell source
code inspection and transformation functionality as well as various
other features that may be useful for an IDE.

Most of Scion's functionality is based on the GHC API.  Scion tries to
be front-end agnostic; it provides both a Haskell API and servers for
non-Haskell clients such as Emacs or Vim.

  [home]: http://code.google.com/p/scion-lib/


Installation
============

Scion requires [GHC 6.10.1][ghc] or later.  All other dependencies
should be on [Hackage] and can be installed using
[cabal-install]:

    $ cd dir/to/scion
    $ cabal install

Scion supports various configuration flags which are useful when
working on Scion itself.

  [ghc]: http://haskell.org/ghc/download.html
  [Hackage]: http://hackage.haskell.org/packages/hackage.html
  [cabal-install]: http://hackage.haskell.org/trac/hackage/wiki/CabalInstall

Usage
=====

TODO

Emacs
-----

TODO

    $ cd <scion>
    $ cabal install -femacs
    $ ./.cabal/bin/emacs_server

Emacs:

    (add-to-list 'load-path "<scion>/emacs")
    (require 'scion)
    
    (add-hook 'haskell-mode-hook 'my-scion-hook)
    (defun my-scion-hook ()
      (scion-mode 1))
    
    M-x scion-connect
    M-x scion-load-cabal-project
    M-x scion-load-library

    C-c i l  -- insert language pragma
    C-c i p  -- insert pragma
    C-c i m  -- insert (external) module name


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

The main repository for Scion is hosted on [Github].  Get it via

    $ git clone git://github.com/nominolo/scion

Send patches or pull requests to nominolo (email address at googlemail
dot com).  Note that, if you fork the project on Github your fork
won't take up additional space on your account.

  [Github]: http://github.com
