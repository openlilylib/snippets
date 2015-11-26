\version "2.19.32"

% Proof-of-concept for a "just intonation" input syntax

% predicate for the detune argument: either a ratio or a cent detune
#(define (number-or-fraction? value)
   (or (number? value)
       (fraction? value)))

ji =
#(define-music-function (pitch dur detune)
   ;; pass a pitch, an optional duration and the detune value
   (ly:pitch? (ly:duration?) number-or-fraction?)
   (let
    ;; if no duration is given, generate one
    ; NOTE: Currently a quarter note is returned
    ((dur (or dur (ly:make-duration 2)))
     (detune-string
      (if (number? detune) (number->string detune)
          (format "~a:~a" (car detune)(cdr detune)))))
    ; so far only the simple note is returned.
    ; eventually the note has to be assigned the appropriate accidental.
    (make-music
          'NoteEvent
          'articulations
            (list (make-music
                   'TextScriptEvent
                   'text (format "~a" detune-string)))
          'pitch pitch
          'duration dur)))

