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

Note: as of 2013-09-06, this is still in very early stage
of development.
