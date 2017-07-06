\version "2.18.0"

\header {
  snippet-title = "Fast fuzzy scale gesture using repeated stems"
  snippet-author = "Piaras Hoban"
  snippet-source = "http://lists.gnu.org/archive/html/lilypond-user/2014-03/msg00680.html"
  snippet-description = \markup {
    Create a gesture from a beamed group of headless stems, with an explicit starting pitch.
    Arguments:
    - data
      (pair of numbers)
      - number of stems
      - x-offset to adjust the gap between the first and the second stem
    - music
      (two-notes chord)
      - first and second notes define the range of the gesture
      - duration of the chord specifies the duration of the gesture

  }
  tags = "contemporary notation, shorthand, gesture, scale"
  status = "unfinished"
  %{
    TODO:
    - Make Beam.positions settable through arguments
    - add optional slash
    - add an optional argument to specify the ratio between the gesture's length
    - add option for specifying duration/extent with a differently-sided note
      and a consecutive spacer rest (may be more straightforward than using explicit spacers?)???
    - maybe add an option to make a non-linear gesture
      This could for example be done through adding notes to the chord

    - Add source comments
    - Maybe clean up the formatting

    NOTE:
    - Doesn't seem to work with relative mode!!!
    - Dotted values don't work for gesture length!
    - Point and click doesn't work with the results of this snippet
  %}
}

% Here goes the snippet

#(define (moment->rhythm moment)
    (let* ((p (ly:moment-main-numerator moment))
             (q (ly:moment-main-denominator moment))
             (k (- (ly:intlog2 q) (ly:intlog2 p)))
             (dots 0))
     (if (< (ash p k) q) (set! k (1+ k))) ;% (ash p k) = p * 2^k
     (set! p (- (ash p k) q))
     (while (begin (set! p (ash p 1))(>= p q))
        (set! p (- p q))
        (set! dots (1+ dots)))
     (if (> k 6)
       (ly:make-duration 6 0) ; 6 means 64th (max value).
       (let* ((dur (ly:make-duration k dots)) ; no exact duration is suitable for
              (dur-len (ly:duration-length dur)) ; all mom : 2 < 5/4 < 2. So use
              (frac (ly:moment-div moment dur-len))) ; a ratio to adjust dur-len
        (ly:make-duration k dots
           (ly:moment-main-numerator frac) ; frac = 1/1 for moment = 3/4, 7/8 etc ..
           (ly:moment-main-denominator frac))))))

#(define (scheme-override grob property value)
    (make-music
        'ContextSpeccedMusic 'context-type 'Bottom 'element
            (make-music 'OverrideProperty 'once #t 'pop-first #t 'grob-property-path (list property)
            'grob-value value 'symbol grob))
)

#(define (scheme-staccato)
    (make-music 'ArticulationEvent 'articulation-type "staccato")
)

repeat-stems = #(define-music-function (layout props data music)
            (pair? ly:music?)
        (let* (
            (n (car data))
            (x-offset (cdr data))
            (stem-adjust -2)
            (elements (ly:music-property (car (extract-named-music music 'EventChord)) 'elements))
            (notes  (map (lambda (x) (ly:music-property x 'pitch)) elements))
            (steps (map (lambda (x) (ly:pitch-steps x)) notes))
            (pitch-diff (- (second steps) (first steps)))
            (y-offset-step (/ (/ pitch-diff n) 2))
            (scale (iota n 0))
            (scale-pitches (map (lambda (x) (first notes)) scale))

            (mus-length (ly:music-duration-length (first elements)))
            (dur-log (ly:duration-log (moment->rhythm mus-length)))
            (dot-count (ly:duration-dot-count (moment->rhythm mus-length)))

            (num (ly:moment-main-numerator mus-length))
            (denom (ly:moment-main-denominator mus-length))
            (new-mom-length (ly:moment-div mus-length (ly:make-moment n 1)))
            (new-duration (moment->rhythm (ly:make-moment 1 8)))

            (i 0)
            (y-offset-counter (- y-offset-step))
            (tuplet (make-music
                        'SequentialMusic
                        'elements
                        (list
                            (make-music 'SequentialMusic 'elements
                                    (map (lambda (x)
                                        (set! i (+ i 1))
                                        (set! y-offset-counter (+ y-offset-counter y-offset-step))
                                            (make-music 'SequentialMusic 'elements
                                                (list
                                                    (if (< (- n 1) i)
                                                            (scheme-override 'Stem 'Y-offset 0)
                                                            (scheme-override 'Stem 'Y-offset y-offset-counter))
                                                    (scheme-override 'NoteHead 'duration-log dur-log)
                                                    (make-music
                                                    'NoteEvent
                                                    'tweaks
                                                        (if (= i 1)
                                                                (list (cons 'X-extent (cons 0 (+ x-offset 1.3))))
                                                                (list
                                                                    '(no-ledgers . #t)
                                                                    '(transparent . #t)
                                                                    '(X-extent . '(-2 . 0))
                                                                )
                                                        )
                                                        'duration new-duration
                                                        'pitch (if (< (- n 1) i) (second notes) x)
                                                    'articulations
                                                        (cond
                                                            ((= i 1) (list (make-music 'BeamEvent 'span-direction -1)))
                                                            ((= i n) (list (make-music 'BeamEvent 'span-direction 1)))
                                                            ((list ))
                                                    ))))
                                        ) scale-pitches)
                                ))))
        (tuplet
                (cond
                    ((= dot-count 0 ) (ly:music-compress tuplet
                                            (ly:moment-div (ly:make-moment 1 n)
                                                    (ly:moment-div (ly:make-moment 1 8) (ly:make-moment 1 denom)))))
                    ((= dot-count 1 ) (ly:music-compress tuplet (ly:make-moment 3 (/ 24 (/ 8 (/ denom 2))))))
                ))
        )
        #{
            $tuplet
        #}
    )
)
