\version "2.18.2"

%% gridly - simple segmented grid for LilyPond
%% Copyright (C) 2015 - Matteo Ceccarello
%% 
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

\include "grid.ily"

SATBChoir =
#(define-music-function
   (parser location segments)
   (segment-selector?)
   #{
     <<
       \new ChoirStaff <<
         \new Staff = "soprano" <<
           \new Voice \gridGetStructure $segments
           \new Voice = "soprano" \gridGetMusic "soprano" $segments
           \new Lyrics \lyricsto "soprano" \gridGetLyrics "soprano" $segments
         >>

         \new Staff = "alto" \new Voice = "alto" \gridGetMusic "alto" $segments
         \new Lyrics \lyricsto "alto" \gridGetLyrics "alto" $segments

         \new Staff = "tenore" \new Voice = "tenore" \gridGetMusic "tenore" $segments
         \new Lyrics \lyricsto "tenore" \gridGetLyrics "tenore" $segments

         \new Staff = "basso" \new Voice = "basso" \gridGetMusic "basso" $segments
         \new Lyrics \lyricsto "basso" \gridGetLyrics "basso" $segments
       >>
     >>
   #})


rehearsalMidi =
#(define-void-function
   (parser location music name)
   (ly:music? string?)
   (let ((book
          #{
            \book {
              \score {
                \unfoldRepeats <<
                  $music
                  \context Staff = $name {
                    \set Score.midiMinimumVolume = #0.4
                    \set Score.midiMaximumVolume = #0.4
                    \set Staff.midiMinimumVolume = #0.8
                    \set Staff.midiMaximumVolume = #1.0
                    \set Staff.midiInstrument = "acoustic grand"
                  }
                >>
                \midi { }
              }
            }
          #}))
     (ly:book-process book
                      #{ \paper { } #}
                      #{ \layout { } #}
                      (string-append
                       (ly:parser-output-name parser)
                       "-rehearsal-"
                       name))))
