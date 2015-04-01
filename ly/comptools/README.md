# comptools - Compilation Helpers for GNU LilyPond

`comptools` is a LilyPond library that provides functionality manipulating
the *compilation* process in one way or the other. That means its tools are
intended to treat the *presentation* part of a LilyPond compilation, and more
specifically, potentially temporary parts of the presentation that are used
for setting up the working environment or assisting in production workflows
rather than editing the content or the published appearance of an edition.
However, this separation is not exclusive, and there are areas of intersection
with persistent modifications.

The most prominent tools in this library are support for

- partial compilation of scores -  
  which is useful for reducing the time to wait for recompilation
  when working on small parts of a score
- conditional breaks -  
  which can be used to adapt the compilation to one or more manuscripts
  in order to simplify the navigation between manuscript and compiled score.
  However, this can also be used to make the handling of manual breaks
  more straightforward if that should be necessary.

---

`comptools` is maintained by Urs Liska - ul@openlilylib.org