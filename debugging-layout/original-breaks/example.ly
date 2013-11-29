
\version "2.17.27"


% Uncomment the following file to respect the original breaks
% Including manually is necessary for this example.
% However, the idea is having this loaded automatically
\include "definitions.ily"
#(define layout-keep-original-breaks #t)

% Music containing original breaks
{
  \tempo "Original breaks"
  \repeat unfold 6 { \repeat unfold 12 c' \originalBreak \mark "|"}
  \originalPageBreak
  #(define-public layout-keep-original-breaks #f)
  \tempo "No original breaks"
  \repeat unfold 6 { \repeat unfold 12 c' \originalBreak \mark "|"}
  \originalPageBreak
  \tempo "\\originalPageBreak"
  \repeat unfold 6 { \repeat unfold 12 c' \originalBreak \mark "|"}
}