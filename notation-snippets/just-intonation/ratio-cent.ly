\version "2.19.32"

% Maintain the "tonic", starting with a default middle c
#(define ji-tonic (ly:make-pitch 0 0 0))

% Change the tonic from which the notes are taken
jiTonic =
#(define-void-function (tonic)
   (ly:pitch?)
   (set! ji-tonic tonic))

% Maintain a current duration to be used when no duration is given
% This is extremely hacky and will only work in monophonic context
#(define ji-duration (ly:make-duration 2))

% Take a fraction and return the corresponding cent value
#(define (ratio->cent f1 f2)
   (if (eq? 1 f2)
       (begin
        (set! f1 (* f1 2))
        (set! f2 (* f2 2))))
   (* 1200
     (/ (log (/ f1 f2)) (log f2))))

% Take a fraction and return a list with 
% - the pitch index (0 - 12)
% the cent deviation above it
#(define (ratio->cent-deviation f1 f2)
   (let*
    ((octave-cent (ratio->cent f1 f2))
     (parts (string-split 
             (format "~a" (/ octave-cent 100.0))
             #\.))
     (pitch-index (string->number (car parts)))
     (cent-str (cadr parts))
     (cent-positive (string->number 
                     (if (> (string-length cent-str) 2)
                         (string-append 
                          (string-take cent-str 2)
                          "."
                          (substring cent-str 2))
                         cent-str)))
     (cent (if (< cent-positive 50) 
               cent-positive
               (- cent-positive 100)))
     (semitone  (if (eq? cent cent-positive)
                    pitch-index
                    (+ pitch-index 1))
       )
     )
    (cons semitone cent)))

% Map the semitone returned by ratio->cent-deviation 
% to a LilyPond pitch index
#(define (semitones->pitch semitone)
   (let ((index (modulo semitone 12))
         (octave (floor (/ semitone 12))))
     (list 
      octave
      (list-ref 
       '((0 0)   ; c
          (0 1/2) ; cis
          (1 0)   ; d
          (1 1/2) ; dis
          (2 0)   ; e
          (3 0)   ; f
          (3 1/2) ; fis %  \ratioToPitch 2 1
  
          (4 0)   ; g
          (4 1/2) ; gis
          (5 0)   ; a
          (5 1/2) ; ais
          (6 0))   ; b      
       index))))

ratioToPitch =
#(define-music-function (dur ratio)
   ((ly:duration?) fraction?)
   (let*
    ((f1 (car ratio))
     (f2 (cdr ratio))
     (note (ratio->cent-deviation f1 f2))
     (lily-pitch (semitones->pitch (car note)))
     (pitch-ratio 
      (ly:pitch-transpose
       (ly:make-pitch 
        (car lily-pitch)
        (car (second lily-pitch))
        (cadr (second lily-pitch)))
       ji-tonic))
     (cent (cdr note))
     (dir (cond 
           ((>= cent 0) "+")
           (else ""))))
    
    (if dur (set! ji-duration dur))
    
    (make-music
     'NoteEvent
     'articulations
     (list (make-music
            'TextScriptEvent
            'text (format "~a~a" dir (round cent))))
     'pitch
     pitch-ratio
     'duration
     ji-duration)))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Here come the examples
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\layout {
  \context {
    \Voice
    \override TextScript.font-size = #-2
  }
  \context {
    \Staff
    \accidentalStyle dodecaphonic
  }
}

#(display "Display Cents within the octave")#(newline)
#(display (ratio->cent 4 3))#(newline)
#(display (ratio->cent 3 2))#(newline)
#(display (ratio->cent 9 8))#(newline)#(newline)

#(display "Display semitone index (0-11) and Cent deviation")#(newline)
#(display (ratio->cent-deviation 4 2))#(newline)
#(display (ratio->cent-deviation 3 2))#(newline)
#(display (ratio->cent-deviation 9 8))#(newline)#(newline)

#(display "Display the corresponding LilyPond code for pitch")#(newline)
#(display (semitones->pitch 1))#(newline)
#(display (semitones->pitch 3))#(newline)
#(display (semitones->pitch 11))#(newline)
#(display (semitones->pitch 12))#(newline)
#(display (semitones->pitch -3))#(newline)


% Print the nearest pitch below the actual pitch
% and print the deviation in Cent below the staff

\markup "A kind of scale over the middle C"

{
  \ratioToPitch 1/1  
  \ratioToPitch 9/8  
  \ratioToPitch 8/7  
  \ratioToPitch 7/6  
  \ratioToPitch 6/5  
  \ratioToPitch 5/4
  \ratioToPitch 4/3
  \ratioToPitch 3/2
  \ratioToPitch 2/1
}

\markup "Overtones over different fundamentals"

{

  \jiTonic f
  \ratioToPitch 2 3/1
  \jiTonic c
  \ratioToPitch 4/1
  \jiTonic as,
  \ratioToPitch 5/1
  \jiTonic f,
  \ratioToPitch 6/1
  \jiTonic d,
  \ratioToPitch 7/1
  \jiTonic c,
  \ratioToPitch 8/1
}

\markup "Overtone scale on different fundamentals"

#(set! ji-duration (ly:make-duration 2))

scale =
#(define-music-function (pitch)(ly:pitch?)
   #{
     \jiTonic #pitch
     \ratioToPitch 1/1
     \ratioToPitch 2/1
     \ratioToPitch 3/1
     \ratioToPitch 4/1
     \ratioToPitch 5/1
     \ratioToPitch 6/1
     \ratioToPitch 7/1
   #})

{
  \clef bass
  \scale a,,
  \scale d,
  \clef treble
  \scale a
}

