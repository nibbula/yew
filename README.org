This is my pile of Common Lisp software. If you are a Lisp programmer, you
have likely done something similar. You may find it interesting to look at my
particular pile of junk. Since it's _my_ pile of junk, I happen to like it.

What I've mostly been up to, is:

* I am trying to build a shell, that is also a Lisp REPL.
  I'm quite used to typing things at a command line.

  If you want to try it, just (asdf:load-system :lish) with ASDF somehow
  configured to look in this directory. External requirements are:
    * cl-ppcre
    * cffi (with sub-dependencies: babel trivial-features alexandria)

  As you may know, an easy way to get the requirements is with quicklisp.

Most of the things are subtly interdependent. And many things are repeating
things now available in other libraries in quicklisp (but perhaps not when I
started). I would like to remedy that, but it means separating things out into
separate libraries, perhaps creating whole separate projects, and perhaps
posting patches to other appropriate libraries.

Some things that are all mushed up in here right now are:
  | Name          | Description                                               |
  | ------------- | --------------------------------------------------------- |
  | dlib          | Basic random pile junk which most other things depend on  |
  | dlib-misc     | dlib things which are not essential                       |
  | dlib-fancy    | dlib things which have more complex dependencies          |
  | dl-list       | The dreaded doubly-linked list                            |
  | stretchy      | Expandable strings and arrays                             |
  | table         | Generic interface tables-like data                        |
  | char-util     | Things relating to characters                             |
  | opsys         | Interface to operating system functions                   |
  | test          | Test running stuff which I even fail use                  |
  | termios       | Interface to POSIX terminal driver                        |
  | ansiterm      | Functions to do things on ANSI terminals                  |
  | curses        | Interface to the curses library                           |
  | completion    | Command line input completion                             |
  | filter-stream | Streams that filter by applying functions                 |
  | glob          | Shell pattern matching                                    |
  | keymap        | Storing what keys perform what actions                    |
  | fui           | Fake user interface things, using curses                  |
  | pager         | Rudimentary file viewer                                   |
  | puca          | Version control interface                                 |
  | tiny-debug    | Crappy debugger for tiny-repl                             |
  | tiny-repl     | A shabby REPL that uses tiny-rl                           |
  | tiny-rl       | Fake and buggy line editing                               |
  | lish          | Lisp shell                                                |

It all used to work on Linux, OSX, and Solaris, but it probably doesn't
anymore. I'd like it to work on cygwin and even non-cygwin Windows. It has, at
times, worked on SBCL, CCL, Lispworks, Allegro, CLisp, ECL, CMU. It probably
only barely works on OSX and SBCL now.

I'd like to stop using Bash. When I feel like I can do that, then perhaps it
will be the time to clean up the above issues so other people can try to use it.

Whatever license I have it under at the time, I'm probably willing to license
it to you under a different license.
