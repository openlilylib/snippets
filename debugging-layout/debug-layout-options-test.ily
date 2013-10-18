% demo sketch for a file that is included
% when a -ddebug-layout command line option is present.
% This will be deprecated.

debugLayoutOptions =
#(define-void-function (parser location)()
   ;; include preview options depending on the
   ;; presence or absence of command line switches
   (if (ly:get-option 'debug-control-points)
       ;; display control points
       (ly:parser-include-string parser "\\include \"debug-slurs.ily\""))
   (if (ly:get-option 'debug-voices)
       ;; color \voiceXXX music
       (ly:parser-include-string parser "\\include \"voice-colors.ily\""))
   (if (ly:get-option 'debug-skylines)
       ;; display skylines
       ;; -> this is very intrusive, so handle with care!
       ;; should be switched off by default
       (ly:set-option 'debug-skylines #t))
       ;; this name clash has to be resolved!
)

\debugLayoutOptions
