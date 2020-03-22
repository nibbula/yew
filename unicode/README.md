This probably shouldn't be here.
This is redundant.
I still need to figure out what the best course of action is.

These:
  sb-unicode
  cl-unicode
  babel
  uax-15
  uax-**

probably already contain most of what we need, with the exception of
char-width, which isn't a standard thing anyway. And of course UTF-8B is
non-standard and probably unadvisable anyway. But, at least cl-unicode is not
up to date. It is certainly a waste to have multiple copies of unicode data in
your image, and also wasteful to have multiple systems loaded that have
overlapping unicode functionality.
