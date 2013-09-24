% demo sketch for a file that is included 
% when a -ddraft-mode command line option is present

draftModeOptions =
#(define-void-function (parser location)()
   ;; include preview options depending on the
   ;; presence or absence of command line switches
   (if (ly:get-option 'draft-display-control-points)
       ;; display control points
       (ly:parser-include-string parser "\\include \"debug-slurs.ily\""))
   (if (ly:get-option 'draft-voice-colors)
       ;; color \voiceXXX music
       (ly:parser-include-string parser "\\include \"voice-colors.ily\""))
   (if (ly:get-option 'draft-debug-skylines)
       ;; display skylines
       ;; -> this is very intrusive, so handle with care!
       ;; should be switched off by default
       (ly:set-option 'debug-skylines #t))
)

\draftModeOptions