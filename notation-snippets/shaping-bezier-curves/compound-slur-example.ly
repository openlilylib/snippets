%%%%%%%%%%%%%%%%%%%%%%%%
\version "2.19.47"
\language "deutsch"

\include "compound-slurs.ily"

upper = \relative {
  \key d \major
  \clef bass
  s2 r8 d,16 g h d g h
  d8 r s2.
  s4 \voiceTwo h8.(-- c16-- h2--)
}
lower = \relative {
  \key d \major
  \clef bass
  r2
  \override Slur.thickness = 3
  <d' h g=>~-^
  % Uncomment one out of the following lines to compare the
  % default slur with the compound one
  %(
  %%{
  -\compoundSlur \with {
    offsets =       % offsets against the automatic control points
    % of the original, non-compound slur
    #'((0 . -1.5)   ; left starting point
        (-2 . -1)   ; second control point
        (2 . -5)    ; second-to-last control point
        (0 . 0))    % right end point

    inflection-ratio =
    #'(0.6 . 0.65)  % X/Y ratio of the inflection point
    % calculated between the actual end points of the slur

    inflection-angle = 50
    inflection-ratio-right = 0.5
    inflection-ratio-left = 0.25

    ann = ##t 	    % Display control points
  }
  %}
  <<
    {
      <d h g>4
      % Uncomment one of the following lines to see the robustness of the slur
      <c g e>4 \(
      %      <c g e>16 q q q
      <h g>4 \voiceOne cis \)
      \change Staff = upper
      \clef treble \voiceOne d e fis2 )
      \fermata
    }
    \new Voice {
      \voiceTwo
      s2. <g,= e>4 \oneVoice
      <fis h,> <g e> <fis dis>2\fermata
    }
  >>
}
\score {
  <<
    \new PianoStaff <<
      \new Staff = upper \upper
      \new Staff = lower \lower
    >>
  >>
}
%%%%%%%%%%%%%%%%%%%%%%%
