\version "2.17.26"

\header {
  snippet-title = "Metric Modulation Function"
  snippet-author = "Peter Bjuhr"
  snippet-source = "no source"
  snippet-description = \markup {
    Function that creates a markup for metric modulation.
    The user inputs:
    Note-value (uses \note, string)
    Tuplet value (0 for no tuplet, 3 for triplet, number)
    for each side of the metric equation
  }
  % add comma-separated tags to make searching more effective:
  tags = "metric modulation, tempo modulation, metric relationship, 
          metric relationship, tempo equation, metric equation"
% is this snippet ready?  See meta/status-values.md
status = "unknown"
% Thoughts about status and improvements:
% Are the unicode symbols a problem?
% Maybe the arrows need a little padding!?
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define-markup-command (metr-mod layout props
                          notI tupI notII tupII )
   (string? number? string? number?)
   "Insert a metric modulation with user settings"
   (interpret-markup layout props
     #{\markup {
       \tiny {
         \right-column {
           \smaller { #(check-left-tupl tupI) }
           \concat {
             ←
             \note #notI #0.9
         } }\center-column { \line {" "} \line { = } }
         \left-column {
           \smaller { #(check-right-tupl tupII) }
           \concat {
             \note #notII #0.9
             →
       } } } }
     #}))

#(define (check-left-tupl ltup)
   (if (= ltup 0) " "
       (string-append (number->string ltup) "⌝")))
#(define (check-right-tupl rtup)
   (if (= rtup 0) " "
       (string-append "⌜" (number->string rtup))))

