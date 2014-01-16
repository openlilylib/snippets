

% TODO: add oll/snippets headers, add example.  Review description.


%{
  Adding automatic octaves to a melody
  This macro allows you to get octaves from a sequence of single notes 
  (this is particularly useful in piano music). 
  The function takes two arguments: 
  the first one is a number (#1 adds notes an octave above, #-1 an octave below, etc), 
  the second one is the music to deal with. 
  Beware: the function can handle chords, but tends to reverse the intervals. 
%}

%LSR This function was contributed by Jay Anderson.

%{
  At some time the function provided by Jay Anderson had been broken.
  Following a discussion between Jay Anderson, Jan-Peter Voigt and David Kastrup
  David came up with the following function.
  This makes octavized duplicates of the music given as argument.
  The music doesn't have to be monophonic anymore, polyphony is
  now also handled correctly.
  The function takes any integer as argument (i.e. also other than one octave)
  and can also be nested to make 4, 8 ... voices
  
  Attention:
  If the music contains slurs or phrasingSlurs 
  one receives a warning about duplicate slurs for 
  any given curve.
%}

\version "2.17.3"

makeOctaves =
#(define-music-function (parser location arg mus)
   (integer? ly:music?)
   #{ <<
     \withMusicProperty #'to-relative-callback
     #(lambda (m p)
        (let ((mu (ly:music-property m 'element)))
          (ly:music-transpose mu (ly:make-pitch (- arg) 0 0))
          (ly:make-music-relative! mu p)
          (ly:music-transpose mu (ly:make-pitch arg 0 0)))
        p)
     \transpose c' $(ly:make-pitch arg 0 0) $mus
     $mus
      >>
   #})


%{
  Usage:
  \relative c {
    \time 3/8
    \key gis \minor
    \makeOctaves #1  { dis8( e dis')~ dis8.( cis16 b8) }
    \makeOctaves #2 { ais8. gis32_| fis^| e8^._-  }
    \makeOctaves #2 {
      \makeOctaves #-1 { 
        ais'( gis-. dis-.) cis( dis <dis gis'>) 
      <e g bes> <f d'>_^^. <fis cis' gis'>_.^- 
      }
    }
  }
%}
