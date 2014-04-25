\include "definitions.ily"

\paper {
  indent = 0\cm
  ragged-right = ##t
}

% Black mensural notation by Lukas Pietsch
% Examples taken from the PDF manual of the LilyPond 2.12 version

% This file is for testing the library and to make it work with current LilyPond.
% Create a variable 'music' with the content of the example and include "testscore.ily"
% which creates the necessary context for the example.


\markup "Page 1"

music = \mensura #'((tempus . #t)(prolatio . #f)) {
      \clavis #'c #3
      \relative c' { c\breve a1 c1 b2 c2 d\breve s\breve }
      \linea "|"
    }

\include "testscore.ily"


%%%%%%%%%%%%%%%%%%%%%%%%%
\markup "Page 3"

music = {
  \mensura #'((tempus . #f)(color . black)) {
    \clavis #'c #3
    \relative c' { e\maxima d\longa c\breve b1 a2 a2 e'4 d c d8 e e1 }
    \linea "|"
  }
  \mensura #'((tempus . #f)(color . white )(unflagged_sm . #t)) {
    \clavis #'c #3
    \relative c' { e\maxima d\longa c\breve b1 a2 a2 e'4 d c b8 a b1 }
    \linea "|"
  }
}

\include "testscore.ily"


%%%%%%%%%%%%%%%%%%%%%%%%%
\markup "Page 3/4"

music = \mensura #'((tempus . #t)) {
  \clavis #'c #3
  \relative c' {
    c\breve
    b1 c1 d1.*2/3
    e1 d c
    e\breve
  }
  \linea "|"
}

\include "testscore.ily"


%%%%%%%%%%%%%%%%%%%%%%%%%
\markup "Page 5/6"

music = {
  \mensura #'((modus . #f)) {
  c'\longa \pausa #2 r\maxima \pausa #1 r\maxima
  c'\longa \pausa #1 r\longa
  c'\longa \linea "|"
  }
  \mensura #'((modus . #t)) {
    c'\longa \pausa #2 r\maxima \pausa #1 r\maxima
    c'\longa \pausa #1 r\longa
    c'\longa \linea "|"
  }
  \mensura #'((modus . #f)(maximodus . #t)) {
    c'\longa \pausa #2 r\maxima \pausa #1 r\maxima
    c'\longa \pausa #1 r\longa
    c'\longa \linea "|"
  }
  \mensura #'((modus . #t)(maximodus . #t)) {
    c'\longa \pausa #2 r\maxima \pausa #1 r\maxima
    c'\longa \pausa #1 r\longa
    c'\longa \linea "|"
  }
}

\include "testscore.ily"

%%%%%%%%%%%%%%%%%%%%%%%%%
\markup "Page 5/6"

music = \relative c' {
  \key f \major
  a1 bes d es d c \accidens b c bes g \accidens fis a\breve
}

\include "testscore.ily"


music = {
  \ligatura { b\breve^BL d\longa }

  \linea "|"
}

\include "testscore.ily"