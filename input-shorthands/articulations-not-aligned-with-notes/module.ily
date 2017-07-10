\version "2.17.25"

\header {
  %FIXME: better name?
  snippet-title = "Articulations not aligned to notes"
  snippet-author = "David Kastrup"
  snippet-description = \markup {
    Small function that simplifies adding dynamics,
    hairpins, articulations and other things in the middle
    of the notes' (or music expressions') durations.
  }
  tags = "syntax, articulation, hairpin, at"
  status = "official"
}

at =
#(define-music-function (parser location t e m)
   (ly:duration? ly:music? ly:music?)
   #{ << #m { \skip $t <>$e } >> #})
