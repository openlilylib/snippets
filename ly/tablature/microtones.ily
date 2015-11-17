\version "2.18.2"
%{
- Author: Thomas Morley
- License: Public domain
- Latest version originally published on:
  http://lists.gnu.org/archive/html/lilypond-user/2015-10/msg00637.html
- Feature added in version 2.19.31:
  https://sourceforge.net/p/testlilyissues/issues/4643/
%}

\header {
  oll-title = "Microtones in tablature"
  oll-description = \markup \wordwrap {
    Print microtones in TabStaff.
    Use cases: bending notation and tunings containing a microtone.
  }
}

#(define-public
  (my-determine-frets context notes specified-info . rest)
  "Determine string numbers and frets for playing @var{notes}
as a chord, given specified information @var{specified-info}.
@var{specified-info} is a list with two list elements,
specified strings @code{defined-strings} and
specified fingerings @code{defined-fingers}.  Only a fingering of@tie{}0
will affect the fret selection, as it specifies an open string.
If @code{defined-strings} is @code{'()}, the context property
@code{defaultStrings} will be used as a list of defined strings.
Will look for predefined fretboards if @code{predefinedFretboardTable}
is not @code {#f}.  If @var{rest} is present, it contains the
@code{FretBoard} grob, and a fretboard will be
created.  Otherwise, a list of @code{(string fret finger)} lists will
be returned.
Quarter-tones are supported, but the printed output may not be very helpful,
unless a reasonable drawing-routine is set for
@code{TabNoteHead.before-line-breaking}"

  ;;  helper functions

  (define (string-frets->placement-list string-frets string-count)
    "Convert @var{string-frets} to @code{fret-diagram-verbose}
dot placement entries."
    (let* ((placements (list->vector
                        (map (lambda (x) (list 'mute  x))
                             (iota string-count 1)))))

      (for-each (lambda (sf)
                  (let* ((string (car sf))
                         (fret (cadr sf))
                         (finger (caddr sf)))
                    (vector-set!
                     placements
                     (1- string)
                     (if (= 0 fret)
                         (list 'open string)
                         (if finger
                             (list 'place-fret string fret finger)
                             (list 'place-fret string fret))))))
                string-frets)
      (vector->list placements)))

  (define (placement-list->string-frets placement-list)
    "Convert @var{placement-list} to string-fret list."
    (map (lambda (x) (if (eq? (car x) 'place-fret)
                         (cdr x)
                         (list (cadr x) 0)))
         (filter (lambda (l) (or (eq? (car l) 'place-fret)
                                 (eq? (car l) 'open)))
                 placement-list)))

  (define (entry-count art-list)
    "Count the number of entries in a list of articulations."
    (length (filter (lambda (x) (not (null? x)))
                    art-list)))

  (define (string-number event)
    "Get the string-number from @var{event}.  Return @var{#f}
if no string-number is present."
    (let ((num (ly:event-property event 'string-number)))
      (and (integer? num) (positive? num) num)))

  (define (determine-frets-and-strings
           notes
           defined-strings
           defined-fingers
           minimum-fret
           maximum-stretch
           tuning)
    "Determine the frets and strings used to play the notes in
@var{notes}, given @var{defined-strings} and @var{defined-fingers}
along with @var{minimum-fret}, @var{maximum-stretch}, and
@var{tuning}.  Returns a list of @code{(string fret finger) lists."


    (define restrain-open-strings (ly:context-property context
                                                       'restrainOpenStrings
                                                       #f))
    (define specified-frets '())
    (define free-strings (iota (length tuning) 1))

    (define (calc-fret pitch string tuning)
      "Calculate the fret to play @var{pitch} on @var{string} with
@var{tuning}."
      (* 2  (- (ly:pitch-tones pitch) (ly:pitch-tones (list-ref tuning (1- string))))))

    (define (note-pitch note)
      "Get the pitch (in semitones) from @var{note}."
      (ly:event-property note 'pitch))

    (define (note-finger ev)
      "Get the fingering from @var{ev}.  Return @var{#f}
if no fingering is present."
      (let* ((articulations (ly:event-property ev 'articulations))
             (finger-found #f))
        (for-each (lambda (art)
                    (let* ((num (ly:event-property art 'digit)))

                      (if (and (ly:in-event-class? art 'fingering-event)
                               (number? num)
                               (> num 0))
                          (set! finger-found num))))
                  articulations)
        finger-found))

    (define (delete-free-string string)
      (if (number? string)
          (set! free-strings
                (delete string free-strings))))

    (define (close-enough fret)
      "Decide if @var{fret} is acceptable, given the already used frets."
      (every (lambda (specced-fret)
               (or (zero? specced-fret)
                   (zero? fret)
                   (>= maximum-stretch (abs (- fret specced-fret)))))
             specified-frets))

    (define (string-qualifies string pitch)
      "Can @var{pitch} be played on @var{string}, given already placed
notes?"
      (let* ((fret (calc-fret pitch string tuning)))
        (and (or (and (not restrain-open-strings)
                      (zero? fret))
                 (>= fret minimum-fret))
             (integer? (truncate fret))
             (close-enough fret))))

    (define (open-string string pitch)
      "Is @var{pitch} and open-string note on @var{string}, given
the current tuning?"
      (let* ((fret (calc-fret pitch string tuning)))
        (zero? fret)))

    (define (set-fret! pitch-entry string finger)
      (let* ((this-fret (calc-fret (car pitch-entry)
                                   string
                                   tuning)))
        (if (< this-fret 0)
            (ly:warning (_ "Negative fret for pitch ~a on string ~a")
                        (car pitch-entry) string))
        (delete-free-string string)
        (set! specified-frets (cons this-fret specified-frets))
        (list-set! string-fret-fingers
                   (cdr pitch-entry)
                   (list string this-fret finger))))

    (define (kill-note! string-fret-fingers note-index)
      (list-set! string-fret-fingers note-index (list #f #t)))

    (define string-fret-fingers
      (map (lambda (string finger)
             (if (null? finger)
                 (list string #f)
                 (list string #f finger)))
           defined-strings defined-fingers))

    ;;; body of determine-frets-and-strings
    (let* ((pitches (map note-pitch notes))
           (pitch-alist (map cons pitches (iota (length pitches)))))

      ;; handle notes with strings assigned and fingering of 0
      (for-each
       (lambda (pitch-entry string-fret-finger)
         (let* ((string (list-ref string-fret-finger 0))
                (finger (if (= (length string-fret-finger) 3)
                            (list-ref string-fret-finger 2)
                            '()))
                (pitch (car pitch-entry))
                (digit (if (null? finger)
                           #f
                           finger)))
           (if (or (not (null? string))
                   (eqv? digit 0))
               (if (eqv? digit 0)
                   ;; here we handle fingers of 0 -- open strings
                   (let ((fit-string
                          (find (lambda (string)
                                  (open-string string pitch))
                                free-strings)))
                     (if fit-string
                         (set-fret! pitch-entry fit-string #f)
                         (ly:warning (_ "No open string for pitch ~a")
                                     pitch)))
                   ;; here we handle assigned strings
                   (let ((this-fret
                          (calc-fret pitch string tuning))
                         (handle-negative
                          (ly:context-property context
                                               'handleNegativeFrets
                                               'recalculate)))
                     (cond ((or (and (>= this-fret 0) (integer? this-fret))
                                (eq? handle-negative 'include))
                            (set-fret! pitch-entry string finger))
                           ((eq? handle-negative 'recalculate)
                            (begin
                              (ly:warning
                               (_ "Requested string for pitch requires negative fret: string ~a pitch ~a")
                               string
                               pitch)
                              (ly:warning (_ "Ignoring string request and recalculating."))
                              (list-set! string-fret-fingers
                                         (cdr pitch-entry)
                                         (if (null? finger)
                                             (list '() #f)
                                             (list '() #f finger)))))
                           ((eq? handle-negative 'ignore)
                            (begin
                              (ly:warning
                               (_ "Requested string for pitch requires negative fret: string ~a pitch ~a")
                               string
                               pitch)
                              (ly:warning (_ "Ignoring note in tablature."))
                              (kill-note! string-fret-fingers
                                          (cdr pitch-entry))))))))))
       pitch-alist string-fret-fingers)
      ;; handle notes without strings assigned -- sorted by pitch, so
      ;; we need to use the alist to have the note number available
      (for-each
       (lambda (pitch-entry)
         (let* ((string-fret-finger (list-ref string-fret-fingers
                                              (cdr pitch-entry)))
                (string (list-ref string-fret-finger 0))
                (finger (if (= (length string-fret-finger) 3)
                            (list-ref string-fret-finger 2)
                            '()))
                (pitch (car pitch-entry))
                (fit-string
                 (find (lambda (string)
                         (string-qualifies string pitch))
                       free-strings)))
           (if (not (list-ref string-fret-finger 1))
               (if fit-string
                   (set-fret! pitch-entry fit-string finger)
                   (begin
                     (ly:event-warning
                      (list-ref notes (cdr pitch-entry))
                      (_ "No string for pitch ~a (given frets ~a)")
                      pitch
                      specified-frets)
                     (kill-note! string-fret-fingers
                                 (cdr pitch-entry)))))))
       (sort pitch-alist (lambda (pitch-entry-a pitch-entry-b)
                           (ly:pitch<? (car pitch-entry-b)
                                       (car pitch-entry-a)))))
      string-fret-fingers)) ;; end of determine-frets-and-strings

  (define (get-predefined-fretboard predefined-fret-table tuning pitches)
    "Search through @var{predefined-fret-table} looking for a predefined
fretboard with a key of @var{(tuning . pitches)}.  The search will check
both up and down an octave in order to accomodate transposition of the
chords.  Returns a placement-list."

    (define (get-fretboard key)
      (let ((hash-handle
             (hash-get-handle predefined-fret-table key)))
        (if hash-handle
            (cdr hash-handle)  ; return table entry
            '())))

    ;; body of get-predefined-fretboard
    (let ((test-fretboard (get-fretboard (cons tuning pitches))))
      (if (not (null? test-fretboard))
          test-fretboard
          (let ((test-fretboard
                 (get-fretboard
                  (cons tuning (map (lambda (x) (shift-octave x 1)) pitches)))))
            (if (not (null? test-fretboard))
                test-fretboard
                (get-fretboard
                 (cons tuning (map (lambda (x) (shift-octave x -1))
                                   pitches))))))))

  ;; body of determine-frets
  (let* ((predefined-fret-table
          (ly:context-property context 'predefinedDiagramTable))
         (tunings (ly:context-property context 'stringTunings))
         (string-count (length tunings))
         (grob (if (null? rest) '() (car rest)))
         (pitches (map (lambda (x) (ly:event-property x 'pitch)) notes))
         (defined-strings (map (lambda (x)
                                 (if (null? x)
                                     x
                                     (or (string-number x) '())))
                               (car specified-info)))
         (defined-fingers (map (lambda (x)
                                 (if (null? x)
                                     x
                                     (ly:event-property x 'digit)))
                               (cadr specified-info)))
         (default-strings (ly:context-property context 'defaultStrings '()))
         (strings-used (if (and (zero? (entry-count defined-strings))
                                (not (zero? (entry-count default-strings))))
                           default-strings
                           defined-strings))
         (predefined-fretboard
          (if predefined-fret-table
              (get-predefined-fretboard
               predefined-fret-table
               tunings
               pitches)
              '())))
    (if (null? predefined-fretboard)
        (let ((string-frets
               (determine-frets-and-strings
                notes
                strings-used
                defined-fingers
                (ly:context-property context 'minimumFret 0)
                (ly:context-property context 'maximumFretStretch 4)
                tunings)))
          (if (null? grob)
              string-frets
              (create-fretboard
               context grob (string-frets->placement-list
                             (filter (lambda (entry)
                                       (car entry))
                                     string-frets)
                             string-count))))
        (if (null? grob)
            (placement-list->string-frets predefined-fretboard)
            (create-fretboard context grob predefined-fretboard)))))

%% Also see:
%% http://lilypond.1069038.n5.nabble.com/guile-question-3-2-gt-quot-1-1-2-quot-td179440.html
#(define (integer-and-fraction nmbr)
  "Return a list with an integer and a fraction build from an exact number
Example: 47/7 -> (6 5/7)"
  (let* ((wh (truncate nmbr))
         (rem (- nmbr wh))
         (fret-string
           (if (or (zero? wh) (zero? rem))
               (format #f "~s 0" nmbr)
               (format #f "~s ~s" wh (abs rem)))))
      ;; TODO: rethink best output format!
      ;;       change to Andrews proposal?
      ;; string-output, with the need to change 'my-format-tab-note-head':
      ;  fret-string

      ;; list-output:
      (cond ((and (zero? wh) (zero? rem))
             (list nmbr 0))
            ((zero? rem)
             (list wh 0))
            ((zero? wh)
             (list 0 rem))
            (else (list wh rem)))))

%% Better to do formatting not in `my-determine-frets'
%% TODO better coding for all this string->number/number->string
#(define my-format-tab-note-head
  (lambda (grob)
    (display (car (last-pair (ly:grob-property grob 'text))))(newline)

    (let* ((txt (ly:grob-property grob 'text))
           (nmbr (if (null? txt) "" (car (last-pair txt))))
           (string-nmbr (if (string? nmbr) 
                            (string->number nmbr)
                            1)))
     (if (and (string? nmbr) string-nmbr)
         (let* ((val (integer-and-fraction string-nmbr))
                (fret
                  (if (and (zero? (car val)) (not (zero? (cadr val))))
                      ""
                      (number->string (car val))))
                (frac
                  (if (zero? (cadr val))
                      ""
                      (markup
                        #:fontsize -2.5
                        (number->string (cadr val))))))
           (ly:grob-set-property! grob 'text
             #{ \markup \concat \vcenter { #fret #frac } #}))
         txt))))

\layout {
  \context {
    \Score
    noteToFretFunction = #my-determine-frets
  }
  \context {
    \TabStaff
    \override TabNoteHead.before-line-breaking = #my-format-tab-note-head
  }
}

% microtones has been integrated to LilyPond as of 2.19.31,
% therefore we issue a deprecation warning
microtonesCheckLilyVersion =
#(define-void-function (parser location)()
   (if (lilypond-greater-than-or-equal? "2.19.31")
       (oll:warn location "

The module
  tablature.microtones 
has been integrated to LilyPond 2.19.31.  You can still use the openLilyLib version but if you 
constantly use newer LilyPond versions you should consider using the built-in function now.")))

\microtonesCheckLilyVersion
