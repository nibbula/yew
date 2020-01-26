Provides a generic interface an imaginary thing once called a terminal.

It's not fully baked yet, but it's more than half baked. 

Unusual characteristics:
- Doesn't require curses, but can use it.
- Sort of works on Windows, as well as Unix.
- Depends on some junk in the [the town.](https://github.com/nibbula/yew)
  and on [opsys](https://github.com/nibbula/opsys).

Example usage:

```
(with-terminal ()
  (tt-clear)
  (tt-write-string-at 5 10 "Guess what key will exit: ")
  (let ((key (tt-get-key)))
    (tt-write-span `(#\newline "That's " (:bold "correct!") ". " #\newline
                     "It was the " (:magenta ,(char-util:nice-char key))
                     " key." #\newline))))
```

```
(with-terminal ()
  (loop :until (tt-listen-for .05) :do
    (tt-color (aref #(:red :green :blue :yellow) (random 4)) :default)
    (tt-move-to (random (tt-height)) (random (tt-width)))
    (tt-write-char #\X)))
```

There's a fairly big docstring on the :terminal package.

There are no automated tests yet, but there are interactive tests in
terminal-test.
