This is a non-groveling interface to operating system functionality.
**I recommend you don't use it.**

If you are for some reason compelled to use this, the packages are:

opsys:
  Has generic interfaces to, frequently least common denominator, operating
  system functionality. If you can get away with using only this level, it might
  just work on the known systems.

unix:
  Interfaces to Unix/POSIX specific things.

ms:
  Interfaces to Microsoft Windows specific things.
  This is the most incomplete part.

libc:
  Interfaces to standard C library things, which is really only for
  compatability and interfacing with other C based libraries.

##### Dependencies (first-order)

- ASDF
- CFFI (and maybe CFFI-LIBFFI on Windows)
- trivial-gray-streams
- from yew:
    - dlib
    - unicode
    - config

##### How to use it.

- Probably right now it's only usable from inside yew or lish. Sorry.

*Note* that this is developed in [yew](https://github.com/nibbula/yew) and is infrequently updated from there.
