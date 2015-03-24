GridLY - Changelog
==================

 * 0.5.0

   - Add bar number handling. See issue #101
   - Add transpose key handling. See issue #102

 * 0.4.0

   This is a **breaking** release. The public interface changed

   - The following public functions have been refactored
      - `gridTest` -> `gridCompileCell`
      - Add function `gridSetRange`
      - `gridGetMusic` lost an argument. Now it accepts only the string
        specifying the part. The segment range should be set using the new
        function `gridSetRange`.
      - `gridSetStructure` -> `gridSetSegmentTemplate`
      - `gridGetStructure` has been removed in favor of the explicit
        use of a `marks` part.


 * 0.3.0

   - Add integers as segment selectors
   - Now including an empty cell in `\gridGetMusic` will no longer throw
     an error: if the segment was initialized with `\gridSetStructure`, then
     a number of skips of the same duration as the defined structure will be
     used. This allows to create scores containing undefined cells, that is
     useful to check work in progress.
   - Now `\gridGetMusic` returns, besides the music of the requested segment
     range, the opening and the closing.
   - Fix a bug in the examples and the in the templates that made the
     compilation fail with LilyPond 2.19.*
   - add lyrics to `\gridTest`: When compiling files containing single
     cells as standalone with the \gridTest command, lyrics are added
     to the output, if present.


 * 0.2.1

   Documentation and license

   - Write a README
   - Switch from LGPL to GPL
   - Add a multi-file example


 * 0.2.0
   This is a **breaking** release. The public interface changed.

   - Some refactoring of public function names, to make the interface more
     consistent. Now all the public music functions start with `grid`.
      - \displayMusicGrid -> \gridDisplay
      - \checkMusicGrid   -> \gridCheck
      - \initMusicGrid    -> \gridInit
   - remove the \gridVersion function. Version checking should be
     the responsibility of some sort of lilypond "package manager"


 * 0.1.0
   Initial relase, featuring the following public functions:
    - \gridVersion
    - \displayMusicGrid
    - \checkMusicGrid
    - \initMusicGrid
    - \gridSetStructure
    - \gridPutMusic
    - \gridGetMusic
    - \gridGetLyrics
    - \gridGetStructure
    - \gridTest
