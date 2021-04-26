#### Replicas of some the ancient tools.

These are some shoddy replicas of traditional tools. They are not POSIX.
They are not feature complete or particularly performant. But, when run from
Lisp, they have a few advantages:

 - They don't require a separate process.
 - They can accept input and pass results as objects in a pipeline when used
   in a Lisp shell.
 - They can be called from normal Lisp programs.

The they should be designed such that:

  - All their functionality is conveniently available as normal Lisp functions.
  - They each have Lish commands that are convenient for interactive use.
  - Non-interactive features for scripting and use from other programs, should
    be omitted from the commands, and provided by the functions.
  - When possible, the commands should be able to accept and provide a
    reasonable set objects for use in Lisp shell pipelines.
  - If there's no reason to deviate from the POSIX command, don't. Don't do
    anything that would be overly surprising people familiar with POSIX.
  - Use grout for output, so output is adaptable.

The separation between interactive and programmatic invocation, helps
alleviate the tension between scripting and interactive features, allows user
customization, removes clutter from the command arguments, and preserves
compatibility. If you want to change the command line arguments to suite your
use, you can do so easily, and it shouldn't affect anything else.

*Note* that this is depends on [yew](https://github.com/nibbula/yew) and infrequently updated from there.
