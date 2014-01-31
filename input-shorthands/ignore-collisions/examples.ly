% Examples for \ignoreCollision

\version "2.16.2"

\include "definitions.ily"

\markup "This will issue a warning about clashing note columns"

\relative c'' {
  <<
    {
      \voiceTwo
      \ignoreCollision
      c2 ^~ \voiceOne c4. b8
    } \\
    \new Voice {
      \voiceTwo
       <e, g>2 <d f>
    }
  >>
}
  
\markup "Using \\ignoreCollision will suppress this warning"

\relative c'' {
  <<
    {
      \voiceTwo
      c2 ^~ \voiceOne c4. b8
    } \\
    \new Voice {
      \voiceTwo
       <e, g>2 <d f>
    }
  >>
}
  