![merlin completion in gvim](http://88.191.77.33/~rks/merlin-complete.png)

Installing Merlin manually
==========================

Compilation
-----------

Needed: OCaml 4, ocamlfind, ocamlbuild, yojson, menhir

Merlin requires OCaml >=4.00.1 (may work with newer versions, but it is tightly
bound to compiler internals).  
Then ensures that "yojson" and "menhir" are installed and are registered in
ocamlfind.

    $ ./configure
    $ make


Installation
------------

If you don't get any error in step above:

    $ make install 

Installation from git
---------------------

Don't forget to checkout submodules after pulling/cloning:

    $ git submodule init
    $ git submodule update

Installing Merlin with opam
===========================

Needed: [opam](http://opam.ocamlpro.com/doc/Advanced_Install.html),
OCaml 4.00.1 (can be installed via opam typing `opam switch 4.00.1`).

Then, just do :

    $ opam install merlin

Setting-up vim
==============

Makes sure that ocamlmerlin binary can be found in PATH.

The only setup needed is to have the following directories in
vim runtime path (append this to your .vimrc):

    :set rtp+=$SHARE_DIR/ocamlmerlin/vim
    :set rtp+=$SHARE_DIR/ocamlmerlin/vimbufsync

The default configuration can be seen in:  

    $SHARE_DIR/ocamlmerlin/vim/plugin/merlin.vim  

Now you may be interested by *Features and interaction with other plugins*,
*Merlin project* and *Extensions* sections.

Using Vundle
------------

Alternatively you can install vim support using [Vundle](https://github.com/gmarik/vundle).
Add the following to your .vimrc

    Bundle 'def-lkb/merlin', {'rtp': 'vim/merlin/'}
    Bundle 'def-lkb/vimbufsync'

Features and interaction with other plugins
--------------------------------------------

Omnicompletion should be available out-of-box.

The documentation is accessible through `:h merlin.txt` and should provide all
the necessary information on how to set-up merlin with other plugins (
[Supertab](https://github.com/ervandew/supertab), 
[neocomplcache](https://github.com/Shougo/neocomplcache),
[syntastic](https://github.com/scrooloose/syntastic)).
It also lists, and explain, all the available commands.

Misc: description of plugin's files
-----------------------------------

- $SHARE\_DIR/ocamlmerlin/vim -- main vim plugin directory
  - plugin/merlin.vim -- sample configuration
  - autoload/
    - merlin.vim   -- main vim script
    - merlin.py    -- helper script needed by merlin.vim
                      (has to be in the same directory)
  - syntax\_checkers/  
                      -- integration with syntastic (ocaml or omlet)  
                      -- set g:syntastic_ocaml_checkers = ['merlin']  
                      --  or g:syntastic_omlet_checkers = ['merlin']

- $SHARE\_DIR/ocamlmerlin/vimbufsync  
              -- library needed by merlin vim mode to keep buffer synchronized


Upgrading vim plugin from merlin 1.0
------------------------------------

Merlin plugin now relies on another vim helper called vimbufsync.
If you installed from opam and/or archive, just make sure that vimbufsync is in vim runtime path:

    :set rtp+=$SHARE_DIR/ocamlmerlin/vimbufsync

Emacs interface
===============

merlin comes with an emacs interface (file: emacs/merlin.el) that implements a
minor-mode that is supposed to be used on top of tuareg-mode.

To get it working you only to load the file `emacs/merlin.el' of the distribution.

If you installed through opam, a good thing to do is:

    
    (add-to-list 'load-path "$SHARE_DIR/emacs/site-lisp/")
    (require 'merlin)


To use it, you will need

- json.el (available by package.el)  
- auto-complete-mode (optional, available by package.el and the MELPA repository)

Once it is done, to enable merlin in a buffer, just type M-x merlin-mode. If you want merlin to be started on every ML buffer issue:

    (add-hook 'tuareg-mode-hook 'merlin-mode)


Features
--------

The emacs `merlin-mode` make all the features of merlin available
inside emacs, by synchronizing ML buffers with a merlin instance. At
any moment, the instance of merlin knows a part of the buffer that
works like locked zones in proof assistant modes (eg. proofgeneral):
editing inside this zone retracts it to the last valid phrase, and C-c
C-t (`merlin-to-point`) expands it to the current position. Moreover
executing a merlin command also tries to advance the locked zone as
much as possible to contain the point.

Main keybindings:

- `C-c C-t` (`merlin-magic-show-type`) shows the type of the expression
  underpoint. To do so, it tries to compile the current phrase and
  locate the leaf of the typedtree containing the current position: 

  - if it is found, it prints the type of this leaf. Further calls to
    `C-c C-t` will print the types of bigger expressions enclosing the
    position ("go up" in the AST). (`C-u C-c C-t` will go down). For
    instance if you have:
    
        List.map (fun n -> n + 1) [1; 2; 3]
    
    and your cursor is on the `n`, then successive calls to `C-c C-t`
    will print (in this order): `int` (`n`), `int` (`n+1`), `int ->
    int` (`fun n -> n + 1`), and `int list` (the whole statement)

  - if it is not possible (the current phrase cannot be made to
    compile), then an approximation is tried to type the expression
    out of context.

  Moreover, if you use a prefix argument, merlin-mode will try to type the region.

- `C-c C-t` (`merlin-to-point`) will update the locked zone to the current position and report all the errors and warnings found.

- `C-c C-x` (`merlin-next-error`) will jump to the next error and display the error message

- `C-c C-u` (`merlin-refresh`) refreshes merlin's cmis cache (when you recompiled other files)

- `C-c C-r` (`merlin-rewind`) retracts the whole buffer (useful when merlin seems confused)

- `C-c C-l` (`merlin-locate`) tries to find the definition of the ident under point and jumps to it. To go back, use `C-c &` (`merlin-pop-stack`)

- `C-c d` (`merlin-show-type-def`) will print the definition of the
  type of the expression underpoint if any. If the type is compliated
  (eg. `'a -> 'a option`) it will print the definition of the
  codomain.

- `C-c l` (`merlin-use`) loads a findlib package inside merlin

- `C-c r` (`merlin-restart-process`) restarts merlin process (useful when hung)

- `C-c C-n` (`merlin-next-phrase`) moves the point to the beginning of the next phrase

- `C-c C-p` (`merlin-prev-phrase`) moves the point to the beginning of the previous phrase

- `C-c C-l` (`merlin-locate`) jumps to the definition of the ident near the point.

  Moreover, you have regular auto-completion (M-TAB by default with
emacs24) using completion-at-point. There is also auto-complete
integration you can enable by setting merlin-use-auto-complete-mode to
t:

    (setq merlin-use-auto-complete-mode t)

It will enable auto-complete mode with the merlin source in ML
buffers. With this setting, you can use `C-c TAB` to force completion.

Merlin project
==============

When loading a ml file in your editor, merlin mode will search a file named
.merlin in the same directory or in parent directories.

The ".merlin" allows you to integrate merlin with your project.
Each line of this file begin with a command name followed by one argument:
- S \<src-dir\>: add a source directory, used to find \*.ml / \*.mli files
- B \<build-dir\>: add a build directory, used to find \*.cmi files
- PKG \<findlib-pkg\>: load a findlib package and its dependencies in merlin

Directory are either absolute or relative to the directory containing ".merlin"
file.

Extensions
==========

Merlin doesn't support (nor plan to support) Camlp4. However, a few common
extensions are hardcoded:

Lwt
---

Support for lwt, match\_lwt, try\_lwt / finally, for\_lwt, while\_lwt,
if\_lwt and raise\_lwt.  
You need to add lwt package (with ":Use lwt" or "PKG lwt" in .merlin) for
this to work, and it may be necessary to reload buffer for this change to
take effect.

type-conv
---------

A few syntax extensions based on type-conv are supported as well.
Namely :
- sexplib.syntax
- binprot.syntax

Screenshots
===========

- [vim](http://88.191.77.33/~rks/merlin/)
- [emacs](http://kiwi.iuwt.fr/~asmanur/projects/merlin/merlin-emacs.html)
