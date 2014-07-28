Below are my ideas for future developments that may follow predefined instrument
contexts.  However, this won't happen before the function for easy creating custom
contexts is finished and accepted into widespread use.

After creating custom contexts, i'd like to push the idea even further
and create a new abstraction layer: instruments.  An instrument has the
following properties:

- it can consist of one or multiple staves, or it may share a staff with other
  instruments (e.g. Soprano and Alto sharing a staff in a choir).  Staffing changes
  may happen during the piece (i.e. an instrument may temporarily split into
  multiple staves and then they could join)
- similarly with voices
- an instrument (which corresponds almost(?) directly to a musician) always begins
  at the beginning of the piece, and always ends at the end of a piece.  It may be
  resting for long periods of time, but it exists continuously.  This is much unlike
  voices, which - particularly in piano music - pop in and out of existence all time.
  (note that staves sometimes also pop in and out of existence).

  QUESTION: can an instrument split?  What happens when we have Sopranos in the choir
  and they temporarily split into 2 (or even 3) subSopranos?  Are these subsintruments
  of Soprano instrument, or rather all of these should be treated as instruments and 
  it's the Sopranos that are an instrument group sharing a staff?


Problems with instruments:
--------------------------

- if there should be multiple score blocks in the file, how does lilypond know 
  where each instrument belongs?  E.g. we have a piece which traditionally
  would be defined with the following structure:

  \score { something }
  \markup { narrator }
  \score { something }
  \markup { narrator }
  \score { something }
  \markup { narrator }

  If we're to ditch traditional \score blocks and have all the structure inferred
  from instruments, how will lilypond know in which "part" to put each instrument?

- How to handle instruments sharing a staff, AND instruments being partcombined??
