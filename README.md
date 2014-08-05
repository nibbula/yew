This is some kind of crust I am creating around myself, with Common Lisp.
If you are a Lisp programmer, you have likely done something similar.
You may find it interesting to look at my particular pile of junk.
Since it's _my_ pile of junk, I happen to like it.
Nothing here is at all notable yet. I'll let “you know” if it is.

What I've mostly been up to, is:

* I am trying to build a shell, that is also a Lisp REPL.
  I'm kind of used to typing things at a command line.

  If you want to try it, just (asdf:load-system :lish) with ASDF somehow*
  configured to look in this directory. External requirements are:
    * cl-ppcre
    * cffi (with sub-dependencies: babel trivial-features alexandria)
  As you may know, an easy way to get the requirements is with quicklisp.

Pushing the dirt around.
