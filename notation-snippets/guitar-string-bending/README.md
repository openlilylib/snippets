Documentation of string bending
===============================

Introduction
------------

LilyPond doesn't support guitar string bending yet.  This snippet
contains a Scheme file which provides some basic features.  It was
written by Marc Hohl in 2009.

There are currently two open issues in the tracker:

* [issue 3700](https://code.google.com/p/lilypond/issues/detail?id=3700)
  aims at integrating the guitar bending written by Marc in Scheme
  in the LilyPond codebase.
* [issue 1196](https://code.google.com/p/lilypond/issues/detail?id=1196)
  is about adding a new bend engraver, which is the ultimate goal.


Limitations
-----------

* Quarter tone bends are not supported.
* Line breaks are not supported.  If a bending is broken by a line
  break, the file won't compile: the workaround is using \noBreak.
* You can't use hammer-on and pull-off when `\bendOn` is active, because
  this snippet uses and transforms the slur engraver.  This implies that
  you cannot, for instance, start a pull-off right after a bend release.
  This is one of the reasons why a bend engraver is needed (see
  issue 1196 above).
* If you use Staff and TabStaff, you may have to add some more padding
  in order to avoid collisions between the bending interval number and
  the staff:
  ```
  \layout {
    \context {
      \StaffGroup
      \override StaffGrouper.staff-staff-spacing.padding = #5
    }
  }```


Usage
-----

Add this repository (openlilylib/snippets) to the list of include paths
in your editor preferences and put the following include statement at
the top of your file:

    \include "notation-snippets/guitar-string-bending/definitions.ily"

You can activate and deactivate the bending with the following commands:

    \bendOn
    % music here
    \bendOff

Within these commands, the parentheses, normally used to notate slurs,
will notate the bendings.  You can write any bending from a half
tone up to any interval (a number over the bending arrow will show
the interval):

    d8( dis)
    d8( e)
    d8( f)

Bend and release requires the use of a closing parenthesis to close the
bending and a new opening one to start the release:

    d8( e)( d)

Pre-bends - when string is bent before plucking it - are supported
through the `\bendGrace` command and two different \preBend commands:

    \bendGrace { \preBendHold c8( } d2)  r2
    \bendGrace { \preBendRelease c8( d)( } c2)  r2

`\preBendHold` is a simple pre-bend, while `\preBendRelease` allows to
release the bending.

Finally, there are two commands that control how a bending behaves
along the time.  `\bendHold` allows to hold a bend for a longer time
by using ties:

    d8( \holdBend e) ~ e2( d)

`\shiftBend` allows to bend a string in two steps:

    c4( \shiftBend d)( e2)
