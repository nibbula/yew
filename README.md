This is a non-groveling interface to operating system functionality.
I recommend you don't use it.

Since it doesn't grovel, it has at least the advantage that it doesn't require
a C compiler and operating system headers. It has the disadvantage of only
working on fairly specific OS versions, and making it work on something
different or new, requires some work. This has been partially done for Linux,
MacOS, FreeBSD, Solaris, and Windows. The biggest disadvantage is that it breaks
horribly with the slightest hidden change to system calls, kernels, and C
libraries. Such changes seem to be happening continuously.

If you are for some reason compelled to use this, the packages are:

opsys:
  Has generic interfaces to, frequently least common denominator, operating
  system functionality. If you can get away with using only this level, it might
  just work on any supported system.

unix:
  Interfaces to Unix/POSIX specific things.

ms:
  Interfaces to Microsoft Windows specific things.
  This part is incomplete!

libc:
  Interfaces to standard C library things, which is really only for
  compatability and interfacing with other C based libraries.

We are trying to cover mostly the space which is system calls, and some
slightly higher level things that would be in a C library. There are many
many other things in an operating system which this should probably never cover.
Also, we are only adding things that are needed by the "village". We are not
adding things for completeness.

##### How to use it.

- ASDF load it with opsys.asd or toss it in ~/quicklisp/local-projects.

- I would probably recommended to use it with a prefix. To use the O/S
  independent part only, use the the OPSYS package, perhaps with the short
  nos: nickname. To use O/S specific functions, use the prefix for the O/S you
  want to use, such os uos: for Unix, wos: for Windows. To be portable, use
  system specific reader macros, such as #+unix and #+windows, to wrap
  such code.

There's some very incomplete preliminary documentation in doc.org.
