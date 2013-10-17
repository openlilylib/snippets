\version "2.17.25"

\header {
  %FIXME: better name?
  snippet-title = "Articulations not aligned to notes"
  snippet-author = "David Kastrup"
  snippet-description = ""
  tags = "syntax, articulation, hairpin, at"
  status = "undocumented"
}

% put the snippet here:
at =
#(define-music-function (parser location t e m)
   (ly:duration? ly:music? ly:music?)
   #{ << #m { \skip $t <>$e } >> #})

musat =
#(define-music-function (parser location t e m)
   (ly:duration? ly:music? ly:music?)
   #{ << #m { \skip $t <>$e } >> #})

% Question: would it be possible to merge above two functions?
