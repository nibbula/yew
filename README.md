This is a line edtior. It's not done yet and it's broken.
It has egregious dependencies and it's not in Quicklisp.
Also, it's very existence is a temporary hack.

If this, for some strange reason, does not dissuade you from trying it,
the dependencies are:

* [https://github.com/nibbula/useless-pile-of-junk-with-a-catchy-name.git]
  For packages:
    :dlib :dlib-misc :dl-list :stretchy :char-util
    :terminal :terminal-ansi :terminal-curses :fatchar
    :completion :keymap :syntax-lisp
    :unipose

* [https://github.com/nibbula/opsys.git]
  For packages:
    :opsys :termios

which you could check out into your `~/quicklisp/local-projects` if so inclined.

Other dependencies are probably in Quicklisp.
