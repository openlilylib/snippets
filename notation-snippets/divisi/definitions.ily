\version "2.19.13"

\header {
  snippet-title = "Partcombine/divisi framework"
  snippet-author = "Janek Warchoł, David Kastrup, Keith OHara"
  snippet-source = ""
  snippet-description = \markup {
    Briefly describe what the snippet does and how to use it.
  }
  % add comma-separated tags to make searching more effective:
  tags = "partcombine, divisi"
  % is this snippet ready?  See meta/status-values.md
  status = "unfinished"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

together = { \tag #'together <>^\markup \bold "a2" }

soloI =
#(define-music-function (parser location mus) (ly:music?)
   #{
     <<
       \tag #'together <>^\markup \bold SoloI
       \tag #'divI { #mus }
       \tag #'together { #mus }
       \tag #'divII #(mmrest-of-length mus)
     >>
   #})

sharedStems =
#(define-music-function (parser location m1 m2) (ly:music? ly:music?)
   #{
     <<
       \tag divI  { #m1 }
       \tag divII { #m2 }
       \tag together <>^\markup \bold "div"
       \tag together << #m1 #m2 >>
     >>
   #})

voiceDivisi =
#(define-music-function (parser location m1 m2) (ly:music? ly:music?)
   #{
     <<
       \tag divI  { #m1 }
       \tag divII { #m2 }
       \tag together << { \dynamicUp #m1 } \\ { \dynamicDown #m2 } >>
     >>
   #})

staffDivisi =
#(define-music-function (parser location m1 m2) (ly:music? ly:music?)
   #{
     \unset Staff.keepAliveInterfaces
     <<
       \tag divI  { #m1 }
       \tag divII { #m2 }
       \tag together #(skip-of-length m1)
       \tag together #(skip-of-length m2)
     >>
     \set Staff.keepAliveInterfaces = #'()
   #})

divisibleStaff =
#(define-music-function (parser location name music) (string? ly:music?)
   (let ((nameI (string-append name " I"))
         (nameII (string-append name " II")))
     #{
       \new GrandStaff \with {
         \consists "Keep_alive_together_engraver"
       } <<
         \new Staff \with {
           \override VerticalAxisGroup.remove-first = ##t
           \override VerticalAxisGroup.remove-empty = ##t
           \override VerticalAxisGroup.remove-layer = 1
           keepAliveInterfaces = #'()
           instrumentName = #nameI
           shortInstrumentName = #nameI
         }
         \keepWithTag divI \music

         \new Staff \with {
           \override VerticalAxisGroup.remove-first = ##t
           \override VerticalAxisGroup.remove-empty = ##t
           \override VerticalAxisGroup.remove-layer = 1
           keepAliveInterfaces = #'()
           instrumentName = #nameII
           shortInstrumentName = #nameII
         }
         \keepWithTag divII \music

         \new Staff \with {
           instrumentName = #name
           shortInstrumentName = #name
           \override VerticalAxisGroup.remove-layer = 2
         }
         \keepWithTag together \music
       >>
     #}))
