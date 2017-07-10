\version "2.17.25"

\include "oll-core/package.ily"
\loadModule snippets.specific-solutions.bracket-repeats
%\include "./definitions.ily"

%%%%%%%%%%%%%%%%%%%%%
%   USAGE EXAMPLE   %
%%%%%%%%%%%%%%%%%%%%%

\relative c'' {
  \time 8/4
  \override Staff.MeasureCounter.font-encoding = #'latin1
  \override Staff.MeasureCounter.font-size = 0
  \override Staff.MeasureCounter.stencil = #repeat-stencil
  \once \override Staff.MeasureCounter.count-from = 3
  \override Staff.DynamicLineSpanner.padding = 2
  \startMeasureCount
  d!2~\p d16 \noBeam es,\mf bes4 b''!4.\f c,!2.\mp
  \stopMeasureCount
  d!2\p
}

\layout {
  \context {
    \Staff
    \consists #Measure_counter_engraver
    \remove "Time_signature_engraver"
  }
}
