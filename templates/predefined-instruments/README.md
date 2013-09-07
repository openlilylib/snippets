_Dedicated to Graham Percival._

_We were talking about making LilyPond easier to use (when
we met in Waltrop, 2012) and Graham suggested that i make
a file with custom context definitions which we could all
test.  Well, better late than never._

Are you tired of setting staff labels (instrumentNames),
clefs, midi instruments etc. for every staff you create?
This should help you.  The idea is that instead of writing

    \new Staff = tenor \with {
      \consists "Ambitus_engraver"
    } {
      \clef "G_8"
      \dynamicUp
      \set Staff.instrumentName = "Tenor"
      \set Staff.shortInstrumentName = "T"
      
      \music
    }
    
you would just write 

    \new TenorStaff \music
    
And all these settings would be taken from TenorStaff definition.

I hope that one day these definitions will make it to the
official LilyPond distribution.

FAQ: _But I need different defaults!  I name all staves with vocal
parts "Trololo" and use Distortion Guitar as their midiInstruments.
Is there any point in me using these templates?_

**Yes!** Even if you need completely different defaults, you can
take advantage of the structure provided. Note that with this
structure you can effecitively separate layout from content,
by placing all modifications in a `\layout` block (which can
be placed in an `.ily` file).

Example: Suppose that you want all soprano and alto staves to be
named "trololo".  You can write something like this:

    \layout {
      \set SopranoStaff.instrumentName = "trololo"
      \set AltoStaff.instrumentName = "trololo"
    }

save it in a file `mystaffnames.ily` and then include this file
at the top of your music file.  It will label all your
`SopranoStaff`s and `AltoStaff`s as "trololo", regardless of
how many of them do you have in your score, and where they are
(and it won't affect other staves).  Then, if you change your
mind and want to use a different naming, you can just change
the names in `mystaffnames.ily` and all your scores that
`\include` this file will be affected!

**Known issues:** custom contexts don't work with MIDI (ugh!!).
See `midi-bug.ly`.

Note: as of 2013-09-06, this is still in very early stage
of development.
