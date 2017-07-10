\version "2.17.29"
% Prior to 2.17.29, \shapeII ... Slur (i.e. the non-tweak syntax)
% will probably affect only the first slur in each example.

\include "oll-core/package.ily"
\loadModules snippets
#'((notation-snippets shaping-bezier-curves shapeII)
   (notation-snippets shaping-bezier-curves special-cpts-display))
%\include "../shapeII.ily"
%\include "special-cpts-display.ily"

\paper {
  ragged-right = ##t
  indent = 0
  top-markup-spacing.basic-distance = #5
}

\header {
  title = \markup \column {
    \vspace #2
    "Adjusting slurs and ties in LilyPond"
    \vspace #1
  }
  subtitle = "Improvements in \shape"
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup \vspace #2
\markup\large\bold "Introduction"
\markup \vspace #1

\markup \italic {
  (read the introduction if you don't have experience
  with adjusting slurs in LilyPond)
}
\markup \vspace #0.5
\markup \justify {
  Slurs are particularly tricky elements of musical notation,
  because they are neither straight lines (like stems or beams),
  nor glyphs with fixed shape (like noteheads or clefs).
  Slurs – and ties, for that matter – are curves.
}
\markup \vspace #0.5
\markup \justify {
  LilyPond, like most existing notation software, uses Bézier curves
  for typesetting slurs and ties.  Such curves are defined by so-called
  “control-points”; usually four control-points are used for each curve.
  The first and last control-point define the beginning and end of the
  curve, while the middle points define the shape (curvature).
}
\markup \vspace #0.5
\markup \justify {
  If you don't like how a slur (or tie) produced by LilyPond looks,
  you have to alter the control-points that define it.  The most
  straightforward way is to explicitely specify the coordinates of each
  control-point manually, but this is very tedious (and if the layout
  of the score changes, the coordinates you had specified earlier
  will usually no longer make sense, so you will have to adjust them again).
}
\markup \vspace #0.5
\markup \justify {
  A much easier and more robust solution is to modify the default
  control-points' positions calculated by LilyPond.
  In 2012 David Nalesnik wrote a function called "\shape" for doing this.
  This function accepts a list of offsets that are applied to respective
  control-points.  Let's look at an example showing how it's used –
  here's a case where default LilyPond slur looks ugly:
}
\markup \vspace #0.5
\markup\small\typewriter\column {
  "\relative c'' {"
  "  d4( d' b g g,8 f' e d c2)"
  "}"
}
\relative c'' {
  d4( d' b g g,8 f' e d c2)
}
\markup \justify {
  We begin by moving first and last control-points 2.3 staffspaces downward:
}
\markup \vspace #0.5
\markup\small\typewriter\column {
  "\relative c'' {"
  "  \shape #'((0 . -2.3)(0 . 0)(0 . 0)(0 . -2.3)) Slur"
  "  d4( d' b g g,8  f' e d c2)"
  "}"
}
\relative c'' {
  \shape #'((0 . -2.3)(0 . 0)(0 . 0)(0 . -2.3)) Slur
  d4( d' b g g,8  f' e d c2)
}
\markup \justify {
  The ends of the slur are now correct, but the middle is wrong.
  So, we move 2nd control-point "1 staffspace" "to the" left and
  "3 staffspaces up," and the 3rd point 2 staffspaces to the left:
}
\markup \vspace #0.5
\markup\small\typewriter
"\shape #'((0 . -2.3)(-1 . 3)(-2 . 0)(0 . -2.3)) Slur"
\markup \vspace #0.7
\relative c'' {
  \shape #'((0 . -2.3)(-1 . 3)(-2 . 0)(0 . -2.3)) Slur
  d4( d' b g g,8  f' e d c2)
}
\noPageBreak
\markup \justify {
  And we now have a correctly shaped slur.  The function can also be used
  to adjust other curves – just specify appropriate object name
  (Slur, PhrasingSlur, Tie, etc.).
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup \vspace #1
\markup\large\bold { Making "\shape" even better }
\noPageBreak
\markup \vspace #1
\noPageBreak

\markup \justify {
  Real-life experience confirmed that "\shape" is an indispensable tool –
  I have already used it to create over "a thousand" publication-quality
  slurs.  It was much, much less work than specifying control-points
  manually.
  Nevertheless, I came to the conclusion that the function could
  be improved to make it even more powerful and more robust against
  layout changes.
}
\markup \vspace #0.5
\markup \justify {
  Take this example: first two measures are two nearly identical phrases
  (the difference is just one accidental) which get two drastically different
  slur shapes by default. 3rd measure contains the same phrase as the 2nd,
  but with changed spacing – again we get a different default slur
  (as of LilyPond 2.17.30):
}
\markup \vspace #0.5

SUp = \change Staff = "up"
SDn = \change Staff = "down"

\new PianoStaff <<
  \new Staff = up \relative d {
    \clef G
    \key e \major
    \time 3/16

    \voiceTwo
    \slurUp

    \SDn \times 2/3 { b32( g' b }
    \SUp \times 2/3 { d g e' }
    \times 2/3 { d b g') }
    |
    \SDn \times 2/3 { b,,,32( g' b }
    \SUp \times 2/3 { dis g e' }
    \times 2/3 { d b g') }
    |
    \newSpacingSection
    \override Score.SpacingSpanner #'common-shortest-duration =
    #(ly:make-moment 1 70)
    \SDn \times 2/3 { b,,,32( g' b }
    \SUp \times 2/3 { dis g e' }
    \times 2/3 { d b g') }
  }
  \new Staff = down {
    \clef F
    \key e \major
    \time 3/16

    s16*9
  }
>>

\markup \justify {
  None of these slurs has a satisfactory shape, especially the middle one.
  They could be adjusted using the method described above (by specifying
  how each control-point should be moved relative to its default position),
  but notice that to make all slurs similar, one would have to find three
  different sets of offsets (because each slur's default shape is different).
  What's worse, changes in score layout may have a “butterfly effect”
  on the slurs – they may change how LilyPond would typeset them by default,
  making user's offset values wrong.  Such situation is much less likely
  when using offsets than in case of speficying control-points' coordinates
  explicitely, but it's not unheard of.
}
\markup \vspace #0.5
\markup \justify {
  Improvements to "\shape" described below make it possible to adjust slurs
  more efficiently and in a more generic way.  In particular, you'll see how
  to adjust all three slurs in the example above using just one "\shape".
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup \vspace #2
\markup\large\bold { Summary of changes }
\noPageBreak
\markup \vspace #1
\noPageBreak

\markup {
  There are three main changes in the way how "\shape" works.
}
\markup \vspace #0.5
\markup \justify {
  Firstly, I've introduced some \italic shorthands
  that simply reduce the amount of typing.
}
\markup \vspace #0.5
\markup \column {
  \line {
    "Secondly, there are now many different ways to specify"
    "the position of control-points:"
  }
  "•  offsets relative to default positions (like in old \shape),"
  "•  symmetrical offsets,"
  "•  absolute coordinates,"
  "•  offsets relative to the noteheads,"
  "•  polar corrdinates."
}
\markup \vspace #0.5
\markup \justify {
  Each control-point may be specified using a different method.
}
\markup \vspace #0.5
\markup \justify {
  Since there are so many different ways of specifying control-points'
  positions, it doesn't make sense to call them “offsets”.
  In this document they will be called “instructions”.
  So, "\shape" accepts lists of instructions for each slur,
  with one instruction for each control-point:
  \small\typewriter "\shape #'(instr-1 instr-2 instr-3 instr-4)".
}
\markup \vspace #0.5
\markup \justify {
  Finally, results of multiple "\shape" commands can now accumulate.
}
\markup \vspace #0.5
\markup \justify {
  For compatibility's sake, the improved function is currently named
  "\shapeII".  I intend to add all these improvements to "\shape" function
  that is included in LilyPond, but before doing this I'd like them
  to be used for some time – maybe something will need adjustments.
  If I make any backward-incompatible changes, I will name the updated
  versions "\shapeIII", "\shapeIV" etc. to avoid messing up users' scores.
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

separator = \markup \hspace #2

\markup { From now on, I'll use the following convention: }
\noPageBreak \markup \vspace #0 \noPageBreak
\markup  {
  \bold LEFT: default LilyPond curve \hspace #5
  \bold RIGHT: result after applying "\shape"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup \vspace #1
\markup\large\bold "Shorthands"
\markup \vspace #1

\markup \justify {
  When you want to change just one control-point, typing
  \typewriter "(0 . 0)" for all remaining points is very tedious.
  Now you can use \typewriter "()" shorthand for every point that
  should remain unchanged. For example,
}
\markup \vspace #0
\markup {
  \small\typewriter "\shapeII #'(()()()(3 . 0))"
  \separator is equivalent to \separator
  \small\typewriter "\shapeII #'((0 . 0)(0 . 0)(0 . 0)(3 . 0))":
}
{
  d''1 ( f'')
  \shapeII #'(()()()(3 . 0)) Slur
  d''1 ( f'')
}

\markup {
  When just one instruction is specified, it's applied to all points:
}
\markup \vspace #0
\markup {
  \small\typewriter "\shapeII #'((3 . 0))"
  \separator is equivalent to \separator
  \small\typewriter "\shapeII #'((3 . 0)(3 . 0)(3 . 0)(3 . 0))":
}
{
  d''1 ( f'')
  \shapeII #'((3 . 0)) Slur
  d''1 ( f'')
}

\markup {
  When just two instructions are specified, they are copied in reverse order:
}
\markup \vspace #0
\markup {
  \small\typewriter "\shapeII #'(foo bar)"
  \separator is equivalent to \separator
  \small\typewriter "\shapeII #'(foo bar bar foo)".
  \separator For example,
}
\markup \vspace #0
\markup {
  \small\typewriter "\shapeII #'((0 . 1)(0 . 2))"
  \separator produces:
}
\markup \vspace #0.2
{
  c''1 ( c'')
  \shapeII #'((0 . 1)(0 . 2)) Slur
  c''1 ( c'')
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup\large\bold "Symmetrical offsets"
\markup \vspace #1
\markup \justify {
  It often happens that you want to shape the curve symmetrically –
  for example, make it shorter.  You could write it like this: \separator
  \small\typewriter "\shapeII #'((2 . 0)(2 . 0)(-2 . 0)(-2 . 0))"
  \separator but it's a lot of typing, and it doesn't look elegant
  (the same number is repeated with different signs).
}
\markup \vspace #0.5
\markup \justify {
  In such situations, use symmetrical offsets, which will be flipped
  horizontally for control-points on the right.  Prefix numbers with
  with \italic symmetrical (or just \italic "s"), e.g.:
  \separator \small\typewriter "\shapeII #'((s 2 0)(s 2 0)(s 2 0)(s 2 0))"
  will shorten the slur by 2 staffspaces on each end. It can be
  written as \separator \small\typewriter "\shapeII #'((s 2 0))":
}
\markup \vspace #0.5
{
  d''1 ( g'' f'')
  \shapeII #'((s 2 0)) Slur
  d''1( g'' f'')
}
\markup \wordwrap {
  \small\typewriter "\shapeII #'((s -2 -1)(s -1 2))"
  \separator is equivalent to \separator
  \small\typewriter "\shapeII #'((-2 . -1)(-1 . 2)(1 . 2)(2 . -1))":
}
\noPageBreak \markup \vspace #0.5 \noPageBreak
{
  d''1 ( d'')
  d''1-\shapeII #'((s -2 -1)(s -1 2)) ( d'')
}

\markup{
  Also, offsets will be flipped vertically depending on curve direction.
  For upward curves
}
\noPageBreak
\markup{
  \small\typewriter "\shapeII #'(()(s 0.5 2))" \separator
  will be equivalent to
}
\noPageBreak
\markup{
  \small\typewriter"\shapeII #'(()(0.5 . 2)(-0.5 . 2)())",
  \separator while for downward curves it will mean
}
\noPageBreak
\markup{
  \small\typewriter"\shapeII #'(()(0.5 . -2)(-0.5 . -2)())":
}
\noPageBreak \markup \vspace #0.5 \noPageBreak
<<
  {
    d''1 ( d'')
    \shapeII #'(()(s 0.5 2)) Slur
    d''1 ( d'')
  }
  \\
  {
    g'1 ( g')
    \shapeII #'(()(s 0.5 2)) Slur
    g'1 ( g')
  }
>>
\pageBreak


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup\large\bold { Absolute coordinates }
\noPageBreak
\markup \vspace #0.5
\noPageBreak
\markup \justify {
  You can explicitely specify the coordinates of a control-point,
  just as if you were directly overriding \small\typewriter control-points
  property. To do this, start the instruction with the
  keyword \italic absolute.
  You can also use \italic abs or even \italic a to save typing
  (all instructions demonstrated here have such shorthands defined).
}
\noPageBreak \markup \vspace #0.5 \noPageBreak
\markup\small\typewriter "\shapeII #'((a 0 0)(a 1 1)(a 5 1)(a 6 0))"
\noPageBreak
{
  g'1 ( d'')
  \shapeII #'((a 0 0)(a 1 1)(a 4 1)(a 5 0)) Slur
  g'1 ( d'')
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup\large\bold { Relative to the noteheads }
\noPageBreak
\markup \vspace #0.5
\noPageBreak

\markup \justify  {
  You can position control-points relative to respective noteheads.
  The offset is measured relative to the notehead center,
  and it is flipped like “symmetrical” offsets.
  First two points will use left notehead as the reference,
  and last two points will use right notehead:
}
\markup \vspace #0.5
\markup\small\typewriter
"\shapeII #'((head 0 0)(head 1 4)(h 2 2)(h 0 1))"
{
  c''1 ( c'')
  \shapeII #'((head 0 0)(head 1 4)(h 2 2)(h 0 1)) Slur
  c''1 ( c'')
}
\markup \justify  {
  You may omit the numbers and just write \small\typewriter (head).
  In that case, the point will be horizontally centered on the notehead,
  and vertically it will be about 0.7 staffspaces away from the notehead:
}
\markup \vspace #0.5
\markup\small\typewriter "\shapeII #'((h)(-1 . 3)(-3 . 0)(h))"
\relative c'' {
  d4( d' b g g,8 f' e d c2)
  \shapeII #'((h)(-1 . 3)(-3 . 0)(h)) Slur
  d4( d' b g g,8  f' e d c2)
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup\large\bold { Mixing the styles }
\noPageBreak
\markup \vspace #0.5
\noPageBreak

\markup {
  As you can see, different ways of specifying coordinates may be mixed:
}
\noPageBreak
\markup \vspace #0.5
\markup\small\typewriter "\shapeII #'((a -0.5 0)(a 0 3)(s 1 1)(h 0 0))"
\noPageBreak \markup \vspace #0.2 \noPageBreak
{
  c''1 ( d'')
  \shapeII #'((a -0.5 0)(a 0 3)(s 1 1)(h 0 0)) Slur
  c''1 ( d'')
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup\large\bold { Polar coordinates }
\markup \vspace #1

\markup \justify {
  Arguably the most powerful way of specifying the positions of middle
  control-points is using polar coordinates.  Use the following syntax:
  \small\typewriter { (polar \concat { \italic "angle radius" ) } }
  (Instead of “polar” you can use “p” to save typing).
  How angle and radius are measured?  Look at the illustration below:
}
{
  \override Slur #'stencil = #(special-control-points)
  e'1( g' e'')
}
\markup \justify {
  You can see a blue line connecting the ends of the slur.  The length of
  this line is what we call \italic "slur length", and the angle between
  this line and the horizontal direction is \italic "slur slope".
  In polar coordinates, radius is measured in slur length;
  angle is measured in degrees, relative to the slur slope.
}
\markup\vspace #0.5
\markup \justify {
  In the following example, the distance between 1st and 2nd control-points
  (as well as between 3rd and 4th) "is 0.5 of the" blue line's length,
  and the red lines (which connect 1st point to the 2nd, and 3rd point
  to the 4th) are perpendicular to the blue line: \separator
  \small\typewriter "\shapeII #'(()(polar 90 0.5)(polar 90 0.5)())"
}
\noPageBreak \markup \vspace #0.2 \noPageBreak
{
  \override Slur #'stencil = #(special-control-points)
  e''1( e'')
  \shapeII #'(()(polar 90 0.5)(polar 90 0.5)()) Slur
  e''1( e'')
}

\pageBreak


\markup \justify {
  Here's another example, showing how the situation changes depending
  on the slur slope – notice the 60 degrees angle between the blue
  and red lines:
}
\noPageBreak \markup \vspace #0.3 \noPageBreak
\markup\small\typewriter"\shapeII #'(()(polar 60 0.5))"
{
  \override Slur #'stencil = #(special-control-points)
  <>_\markup \tiny "<----------------(default)--------------->"
  b'1( b')
  d'( g'')
  <>_\markup \tiny "<---------------(tweaked)-------------->"
  \shapeII #'(()(polar 60 0.5)) Slur
  b'1( b')
  d'( g'')
}

\markup \justify {
  It is possible to measure the angle against the horizontal line
  by using “absolute” polar coordinates:
}
\noPageBreak \markup \vspace #0.3 \noPageBreak
\markup\small\typewriter
"\shapeII #'((0 -2)(absolute-polar 88 0.5)(ap 20 0.2)())"
\markup\vspace #0.2
\score {
  {
    g2( d'' b'' d''')
    \shapeII #'((s 0 -2)(absolute-polar 88 0.5)(ap 20 0.2)()) Slur
    g2( d'' b'' d''')
  }
  \layout { }
}
\markup {
  With polar coords, the same values can be used for different slurs
  to produce very similar shapes:
}
\noPageBreak \markup \vspace #0.3 \noPageBreak
\markup\small\typewriter "\shapeII #'(()(p 30 0.7)(p 90 0.3)())"

{
  <>_\markup \tiny "<----------(default)--------->"
  e''2 ( d'') |
  e''4 ( e'' d'' d'' )
  \bar "||"
  <>_\markup \tiny "<---------(tweaked)-------->"
  \shapeII #'(()(p 30 0.7)(p 90 0.3)()) Slur
  e''2 ( d'')  |
  e''4 ( e'' d'' d'' )
}

\markup \justify {
  You can specify a point's polar coordinates relative to its default
  polar coordinates. For example, this command will place 2nd point
  two times farther and 20 degrees more outwards than default placement:
}
\noPageBreak \markup \vspace #0.3 \noPageBreak
\markup\small\typewriter "\shapeII #'(()(rp 20 2)()()) Slur"
\score {
  {
    b2( d'' a'' b'')
    \shapeII #'(()(rp 20 2)()()) Slur
    b2( d'' a'' b'')
  }
  \layout { }
}

\markup\line {
  S-shaped slurs are easy to achieve with polar coordinates:
}
\noPageBreak \markup \vspace #0.3 \noPageBreak
\markup\small\typewriter "\shapeII #'(()(p -30 0.5)(p 30 0.5)())"
{
  \shapeII #'(()(p -30 0.5)(p 30 0.5)()) Slur a1 ( g)
}

\markup\small\typewriter "\shapeII #'((h 0 1.8)(p 35 0.5)(p -35 0.5)(h 0 -1.5))"
\noPageBreak \markup \vspace #0.3 \noPageBreak
\relative c''{
  c8( e b-> f d' a e-> g)
  \shapeII #'((h 0 1.8)(p 35 0.5)(p -35 0.5)(h 0 -1.5)) Slur
  c8( e b-> f d' a e-> g)
}

\markup {
  As you can see, shorthands work with polar coordinates:
}
\noPageBreak \markup \vspace #0.3 \noPageBreak
\markup\small\typewriter "\shapeII #'(()(p 50 0.35))"
\noPageBreak \markup \vspace #0.3 \noPageBreak
\score {
  {
    d''2-\shapeII #'(()(p 50 0.35)) ( f'' f'' d'')
  }
  \layout { }
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup\large\bold "Accumulation"
\noPageBreak
\markup \vspace #1
\noPageBreak
%{
  What do you mean by accumulation? Does shape now affect all subsequent
  shapes or is it still only affecting the _next_ one?
%}

\markup \justify {
  "\shape" can be written in two forms – \italic "override-like":
}
\noPageBreak
\markup \vspace #0.5
\markup\small\typewriter \column {
  "{"
  "  c''1 ( c'')"
  "  \shapeII #'((0 . 2)) Slur"
  "  c''1 ( c'')"
  "}"
}

\markup \vspace #0.5
\markup \justify {
  and \italic "tweak-like":
}
\noPageBreak
\markup \vspace #0.5
\markup\small\typewriter \column {
  "{"
  "  c''1 ( c'')"
  "  c''1-\shapeII #'((2 . 0)) ( c'')"
  "}"
}

\markup \vspace #0.5
\markup {
  If you use both \italic tweak-like and \italic "override-like" "\shape"
  on a slur, their effects will combine:
}
\markup \vspace #0.5
\markup\small\typewriter \column {
  "{"
  "  \shapeII #'((0 . 2)) Slur"
  "  a'1-\shapeII #'((2 . 0)) ( c'')"
  "}"
}
{
  \shapeII #'((0 . 2)) Slur
  a'1-\shapeII #'((2 . 0)) ( c'')
}

\markup \justify {
  Since LilyPond 2.17.29, the \italic override-like form will affect
  \italic all subsequent curves of the specified type (see
  \with-url #"http://code.google.com/p/lilypond/issues/detail?id=3603"
  "Issue 3603").  For example, this will move all three slurs up:
}
\markup \vspace #0.5
\markup\small\typewriter \column {
  "{"
  "  \shapeII #'((0 . 2)) Slur"
  "  a'1 ( c'')"
  "  a'1 ( c'')"
  "  a'1 ( c'')"
  "}"
}
\markup \vspace #0.5
{
  \shapeII #'((0 . 2)) Slur
  a'1 ( c'')
  a'1 ( c'')
  a'1 ( c'')
}
\markup \justify {
  This gives us interesting possibilites.  If we have a passage of similar
  slurs, we can use the override-like "\shape" to give all the slurs some
  particular appearance, and then use tweak-like "\shape" to fine-tune
  any slurs for which the “general” "\shape" didn't provide satisfactory
  results.  In the example below, first all slurs are moved upwards,
  and then one of them is given more curvature:
}
\markup \vspace #0.5
\markup\small\typewriter \column {
  "{"
  "  \shapeII #'((0 . 1.5)) Slur"
  "  a'1 ( c'')"
  "  a'1 ( c'')"
  "  a'1 ( c'')"
  "  a'1-\shapeII #'(()(rp 20 1.7)) ( c'')"
  "  a'1 ( c'')"
  "}"
}
{
  \shapeII #'((0 . 1.5)) Slur
  a'1 ( c'')
  a'1 ( c'')
  a'1 ( c'')
  a'1-\shapeII #'(()(rp 20 1.7)) ( c'')
  a'1 ( c'')
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup\large\bold "Broken curves"
\noPageBreak
\markup \vspace #1
\noPageBreak
\markup \column {
  "All of this should work for broken slurs/ties as well."
  "If you specify one set of instructions, it will be applied to all siblings:"
}
\noPageBreak
\markup \vspace #0
\noPageBreak
\markup \line {
  \column {
    \vspace #0.5
    default:
    \vspace #0.5
    \score {
      { d''1 ( f'' \break a'' g'') }
      \layout { }
    }
  }
  \hspace #20
  \column {
    \vspace #0.5
    \small\typewriter "\shapeII #'(()(rp 15 2))"
    \vspace #0.5
    \score {
      { d''1-\shapeII #'(()(rp 15 2)) ( f'' \break a'' g'') }
      \layout { }
    }
  }
}


\markup \vspace #1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup\large\bold "Epilogue"
\noPageBreak
\markup \vspace #0.5
\noPageBreak
\markup \justify {
  Equipped with all these features, we can easily fix the bad-looking
  slurs from the example at the beginning.
  Note that just \bold one override gives \bold all slurs correct appearance:
}
\markup \vspace #0.5
\markup \typewriter\small "\shapeII #'((h)(p 55 0.5)(p 50 0.2)(h 0 1.5)) Slur"
\markup \vspace #1

\new PianoStaff <<
  \new Staff = up \relative d {
    \clef G
    \key e \major
    \time 3/16

    \voiceTwo
    \slurUp

    \shapeII #'((h)(p 55 0.5)(p 50 0.2)(h 0 1.5)) Slur

    \SDn \times 2/3 { b32( g' b }
    \SUp \times 2/3 { d g e' }
    \times 2/3 { d b g') }
    |
    \SDn \times 2/3 { b,,,32( g' b }
    \SUp \times 2/3 { dis g e' }
    \times 2/3 { d b g') }
    |
    \newSpacingSection
    \override Score.SpacingSpanner #'common-shortest-duration =
    #(ly:make-moment 1 70)
    \SDn \times 2/3 { b,,,32( g' b }
    \SUp \times 2/3 { dis g e' }
    \times 2/3 { d b g') }
  }
  \new Staff = down {
    \clef F
    \key e \major
    \time 3/16

    s16*9
  }
>>

