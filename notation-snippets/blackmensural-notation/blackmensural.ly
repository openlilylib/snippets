\version "2.12.0"
%% ========================================================
%  blackmensural.ly – Black Mensural Notation for Lilypond 
%  Version 0.1, January 2011
%  (C) Lukas Pietsch
%  Released under the GNU General Public License
%% ========================================================

%% ===============================================
%  Context type definitions
%% ===============================================

%% some settings on score level
mensuralTightSetting = {
  \override Score.SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 2)
  \override Score.SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 2)
  \override Score.SpacingSpanner #'shortest-duration-space = #1.5
  \override Score.SpacingSpanner #'spacing-increment = #0.05
  \override NoteSpacing    #'stem-spacing-correction = #0.0
  \override Score.BarLine #'stencil = #empty-stencil
  \override Score.BarNumber #'stencil = #empty-stencil
}

\layout {
    ragged-right = ##t 
    \context {
      \Voice
      \name BlackMensuralVoice  %% adapted from MensuralVoice definition
      \alias Voice
      \remove Beam_engraver 
      \remove Stem_engraver
      \override Stem #'transparent = ##t
      \override Flag #'transparent = ##t
      \override Beam #'transparent = ##t
      \override Accidental #'stencil = #empty-stencil
      \override Accidental #'font-size = #-2
      \override Accidental #'X-offset = #-2
    }
    \context {
      \Staff
      \name BlackMensuralStaff  %% adapted from MensuralStaff definition
      \alias Staff
      \denies Voice
      \defaultchild BlackMensuralVoice
      \accepts BlackMensuralVoice
      \description "Same as @code{Staff} context, except that it is
    accommodated for typesetting a piece in mensural style."

      \consists "Custos_engraver"

      %% We can not remove Bar_engraver; otherwise clefs and custodes will
      %% not show up any more among other line breaking issues.
      %% Instead, we make the grob invisible
      \override BarLine #'stencil = #empty-stencil
      %\override BarLine #'transparent = ##t

      \override StaffSymbol #'thickness = #0.6
      \override KeySignature #'font-size = #-2
      \override TimeSignature #'font-size = #-2

      %% Choose c clef on 3rd line as default.
      clefGlyph = #"clefs.vaticana.do"
      middleCClefPosition = #0
      middleCPosition = #0
      clefPosition = #0
      clefOctavation = #0

      %% Select mensural style font.
      \override NoteHead #'style = #'mensural   % will be further overridden
      \override TimeSignature #'style = #'mensural
      \override KeySignature #'style = #'mensural
      \override Accidental #'glyph-name-alist = #alteration-mensural-glyph-name-alist
      \override Custos #'style = #'mensural
      \override Custos #'neutral-position = #3
      \override Custos #'neutral-direction = #DOWN
      \override Dots #'font-size = #-3

      %% Accidentals are valid only once (same as
      %% #(set-accidental-style 'forget))
      extraNatural = ##f
      autoAccidentals = #`(Staff ,(make-accidental-rule 'same-octave -1))
      autoCautionaries = #'()  
      printKeyCancellation = ##f


    }
    \context {
      \Score
      \accepts BlackMensuralStaff
    }
}

%% ===============================================
%  declaring some custom properties
%% ===============================================

%% custom NoteHead property, for \override-able notehead style definitions
#(set-object-property! 'mensural_glyphs 'backend-type? list?)
#(set-object-property! 'mensural_glyphs 'backend-doc "current set of mensural display settings")

%% custom Voice context property, needed for ligature assembly
#(set-object-property! 'mensural_ligature_queue 'backend-type? list?)
#(set-object-property! 'mensural_ligature_queue 'backend-doc "current set of note grobs to be assembled into a ligature")
#(set-object-property! 'mensural_accidentals_queue 'backend-type? list?)
#(set-object-property! 'mensural_accidentals_queue 'backend-doc "current set of accidentals to be collected for a ligature")

%% custom entry in an event's 'tweak property
#(set-object-property! 'mensural_tags 'backend-type? list?)
#(set-object-property! 'mensural_tags 'backend-doc "various tags set in a notehead tweak to control mensural note display")


%% ==============================================
%  convenience definition - empty stencil
%% ==============================================
#(define-public empty-stencil
   (ly:make-stencil (list 'embedded-ps "") '(0 . 0) '(0 . 0)))

%% ==============================================
%  convenience shortcut: set a custom flag in our
%  "mensural_tags" tweak property
%% ==============================================
#(define (set-mensural-flag! note key val)
  (let* ((tweaks (ly:music-property note 'tweaks))
         (flags (if tweaks (assq-ref tweaks 'mensural_tags) '())))
    (if (not tweaks) (set! tweaks '()))
    (if (not flags)  (set! flags  '()))
    (set! flags  (assq-set! flags key val))
    (set! tweaks (assq-set! tweaks 'mensural_tags flags))
    (ly:music-set-property! note 'tweaks tweaks)
    note))
#(define (get-mensural-flag note key)
  (let* ((flags (get-tweak note 'mensural_tags)))
    (assq-ref flags key)))
#(define (set-tweak! event key val frc)
  (let* ((tw (ly:music-property event 'tweaks))
         (old (assq key tw)))
    (if (or frc (not old))
      (begin
        (set! tw (assq-set! tw key val))
        (ly:music-set-property! event 'tweaks tw)))
    event))
#(define (get-tweak event key)
  (let* (
    (tw (if (ly:music? event) (ly:music-property event 'tweaks) (ly:event-property event 'tweaks))))
    (assq-ref tw key)))

#(define (flag-notes! key val onlynotes music )
  (let ((test (if onlynotes 'note-event 'rhythmic-event)))
    (music-map
      (lambda (event)
        (begin
          (if (memq test (ly:music-property event 'types))
            (set-mensural-flag! event key val)))
        event)
      music)
    music))
#(define (tweak-notes! key val onlynotes music )
  (let ((test (if onlynotes 'note-event 'rhythmic-event)))
    (music-map
      (lambda (event)
        (begin
          (if (memq test (ly:music-property event 'types))
            (set-tweak! event key val onlynotes)))
        event)
      music)
    music))
flagNotes = #(define-music-function (parser location key val music) (symbol? boolean? ly:music?)
  (flag-notes! key val #t music))


%% =================================================
%  Mark an event as explicitly imperfect. Needed only
%  for imperfect longa rests in perfect modus
%% =================================================
imperfect = #(define-music-function (parser location mymusic) (ly:music?)
  (flag-notes! 'imperfect #t  #f mymusic))
%% =================================================
%  Make a row of "currentes" ("coniunctura") rhombic
%  noteheads inside a ligature
%% =================================================
currentes = #(define-music-function (parser location mymusic) (ly:music?)
  (flag-notes! 'currens #t #t mymusic))
%% =================================================
%  Mark whether a final ascending L in a ligature
%  should be folded in
%% =================================================
pes = #(define-music-function (parser location mymusic) (ly:music?)
  (flag-notes! 'pes #t #t mymusic))
nopes = #(define-music-function (parser location mymusic) (ly:music?)
  (flag-notes! 'nopes #t #t mymusic))
%% =================================================
%  Mark various options for vertical stems
%% =================================================
virga = #(define-music-function (parser location mymusic) (ly:music?)
  (flag-notes! 'altvirga #t #t mymusic))
novirga = #(define-music-function (parser location mymusic) (ly:music?)
  (flag-notes! 'novirga #t #t mymusic))
%% =================================================
%  Mark whether two notes in a ligature
%  should be displayed as an oblique
%% =================================================
obliqua = #(define-music-function (parser location mymusic) (ly:music?)
  (flag-notes! 'obliqua #t #t mymusic))
noobliqua = #(define-music-function (parser location mymusic) (ly:music?)
  (flag-notes! 'noobliqua #t #t mymusic))


%% =================================================
%  Mark one or several notes with a special stencil
%  Needed for Italian trecento and French "mannered"
%  notation, e.g. various "semibreves signatae".
%% =================================================
signata = #(define-music-function (parser location stencilfunction mymusic) (ly:stencil? ly:music?)
 (make-music 'SequentialMusic
   'elements (list
      (make-music                         ; =   \override NoteHead #'stencil = #stencilfunction
        'ContextSpeccedMusic
        'context-type 'Bottom
        'element
        (make-music
          'OverrideProperty
          'symbol 'NoteHead
          'grob-property-path (list 'stencil)
          'grob-value stencilfunction))
      mymusic
      (make-music
        'ContextSpeccedMusic
        'context-type 'Bottom
        'element
        (make-music
          'RevertProperty
          'symbol 'NoteHead
          'grob-property-path (list 'stencil))))))

%% alternative version using a 'stencil tweak, but
%% the override seems to get better horizontal spacing.
signataB = #(define-music-function (parser location stencil mymusic) (ly:stencil? ly:music?)
  (tweak-notes! 'stencil stencil #t mymusic))

%% ==================================================
%  Coloratio
%% ==================================================
coloratio = #(define-music-function (parser location col mymusic) (symbol? ly:music?)
  (begin
    (music-map                 ; iterate through all the music
      (lambda (event)
        (cond 
          ;; for notes: use our custom color flag
          ((memq 'note-event (ly:music-property event 'types))
            (set-mensural-flag! event 'color col))
          ;; for rests: use the standard color mechanism
          ((memq 'rest-event (ly:music-property event 'types))
            (case col
              ((red redhollow)
                (set-tweak! event 'color (rgb-color 0.7 0 0) #f)))))
        event)
      mymusic)
    mymusic))

%% ==============================================
%  LIGATURE CODE
%% ==============================================

%% =========================================================
%  This is the music function that will be called during
%  input processing. It does no processing of its own, but
%  only adds "applyContext" callbacks at the beginning and
%  end of the ligature in the input stream. These, together
%  with the "applyOutput" callbacks inserted by the 
%  \mensura command, will later get executed during translation.
%% =========================================================
ligatura = #(define-music-function (parser location mymusic) (ly:music?)
     (make-music              
      'SequentialMusic
      'elements
      (list
        ;; call to start_ligature callback
        (make-music
          'ApplyContext
          'context-type 'Bottom
          'procedure start_ligature)
        ;; original music
        mymusic
        ;; call to finish_ligature callback
        (make-music
          'ApplyContext
          'context-type 'Bottom
          'procedure finish_ligature) 

        ;; ugly workaround to get horizontal spacing right:
        ;; insert an invisible breathing mark as a dummy grob
        (make-music
          'ContextSpeccedMusic
          'context-type 'Bottom
          'element (make-music
            'OverrideProperty
            'symbol 'BreathingSign
            'grob-property-path '(text)
            'grob-value " "
            'pop-first #t
            'once #t))
        (make-music
          'EventChord
          'elements
          (list (make-music (quote BreathingEvent))))

        )))

%% =========================================================
%  This procedure will be called for every grob in the Voice
%  context during the translation phase, through ApplyOutput
%  calls (placed throughout the music by the \mensura command).
%  It checks whether we are in a ligature, and if yes, places
%  relevant grobs in a queue to be processed at the end of the
%  ligature. It also queries each grob's X-extent property,
%  to force Lilypond to calculate the stencil widths properly.
%% =========================================================
#(define (mensural_processing grob origin context)
  (let* ((ifs   (ly:grob-interfaces grob))
        (cause (ly:grob-property grob 'cause))
        (type  (cdr (assq 'name (ly:grob-property grob 'meta ))))
        (queue (ly:context-property  
                 context
                 'mensural_ligature_queue ))
        (inligature (if queue #t #f))
       )
    ;(display (format #f "We got a ~a\n" type))
    (if inligature
      (cond 
        ((eq? type 'NoteHead)
          (begin
            (ly:context-set-property! 
              context 
              'mensural_ligature_queue 
              (append queue (list grob)))))
        ((eq? type 'Dots)                           
          ;; discard the standard dot grob, but mark the parent note with a flag
          ;; so that the ligature creation procedure can add it as part of the ligature stencil.
          (let* ((lastnote (car (last-pair queue)))
                 (lastflags  (if lastnote (ly:grob-property lastnote 'mensural_tags))))
            (ly:grob-set-property! lastnote 'mensural_tags
              (assq-set! lastflags 'punctum #t))
            (ly:grob-set-property! grob 'stencil empty-stencil)))
        ((eq? type 'Accidental)
          ;; put ligature accidentals into a queue of their own.
          (let* ((stencil (ly:grob-property grob 'stencil))
                 (position (ly:grob-property cause 'Y-offset))
                 (accqueue (ly:context-property context 'mensural_accidentals_queue)))
            (if (eq? accqueue #f) (set! accqueue '()))
            (ly:grob-set-property! grob 'stencil empty-stencil)
            (ly:context-set-property!
              context
              'mensural_accidentals_queue
              (append accqueue (list (cons position stencil)))))))

      ;; need to do this for every note head, or Lilypond won't figure out
      ;; the customized X-width in time.
      (if (or (eq? type 'NoteHead) (eq? type 'Rest))
        (ly:grob-property grob 'X-extent))
    )))


% ===========================================
% convenience function: use explanatory
% symbols rather than duration logs for
% note values
% ===========================================
#(define (duration_log_to_note_symbol log)
  (case log
    ((-3) 'mx)
    ((-2) 'lg)
    ((-1) 'br)
    ((0)  'sb)
    ((1)  'mn)
    ((2)  'sm)
    ((3)  'fu)
    ((4)  'sf)
    (else #f)))

#(define (start_ligature context)
  (ly:context-set-property! 
    context 
    'mensural_ligature_queue 
    '()))

%% ========================================================
%  This procedure will be called after the last notehead
%  grob of each ligature, through ApplyContext. 
%  It recovers the stored grobs from the custom
%  context property 'mensural_ligature_queue, calculates
%  a single postscript stencil for the ligature, assigns
%  this stencil to one of the note grobs, and makes the 
%  others invisible.
%% =========================================================
#(define (finish_ligature context)
  (let* ((findcontext (ly:context-property-where-defined context 'mensural_ligature_queue))
         (notes (if (ly:context? findcontext)
                   (ly:context-property findcontext 'mensural_ligature_queue )
                   '()))
         (imaxall (- (length notes) 1))
         (imax imaxall)
         (imiddle (truncate (/ imax 2)))    ; this is the note we'll assign our customized stencil to.
         (ymiddle (ly:grob-property (list-ref notes imiddle) 'Y-offset))

         (ypositions (map (lambda (gr) (ly:grob-property gr 'Y-offset)) notes) )
         (vals       (map (lambda (gr) (duration_log_to_note_symbol (ly:grob-property gr 'duration-log))) notes))
         (flags      (map (lambda (gr) (ly:grob-property gr 'mensural_tags)) notes))

         (glyphsettings (ly:grob-property (car notes) 'mensural_glyphs))
         (foldpes    (assq-ref glyphsettings 'foldpes))
         (basecolor  (or (assq-ref glyphsettings 'color) 'black))

         (mystencil  #f)
         (postscript "")
         (notewidth 0.75)
         (noteheight 0.75)
         (linewidth 0.11)
         (rightX (* notewidth (+ imaxall 1)))  
         (curX rightX)
         (firstpos (car ypositions))
         (posref 
           (lambda (i)
             (list-ref ypositions i)))
         (add-postscript   
           (lambda (ps) 
             (set! postscript (string-append postscript ps))))
         (shift-postscript
           (lambda (dX dY)
             (begin
               (add-postscript (format #f " ~a ~a translate " dX dY))
               (set! curX (+ curX dX)))))
         (obliqua-width
           (lambda (adiff)
             (+ (* adiff adiff (/ 1.0 12.0)) (* adiff (/ 7.0 12.0)) (/ 4.0 3.0))))
         )

    ; we need at least two notes to proceed beyond here.
    (if (>= imaxall 1)
      (begin

        ;check how many of our notes are regular ligature notes,
        ;and how many are trailing currentes or plicae
        (do ((i 0 (1+ i)))
            ((or (> i imaxall) (< imax imaxall)))
          (if (assq-ref (list-ref flags i) 'currens)
             (set! imax (- i 1)))
          (if (assq-ref (list-ref flags i) 'plica)
            (begin
              (set! imax (- i 1))
              ; also check whether the preceding note is manually forced to be folded in 
              (if (and 
                   (> i 0)
                   (assq-ref (list-ref flags (- i 1)) 'pes))
                  (set! foldpes #t)
                  (set! foldpes #f)))))

        ;(display (format #f "Finishing ligature with ~a (~a) notes.\n" (+ 1 imaxall) (+ 1 imax)))

        ;; ===========================
        ;; main loop through note list
        ;; ===========================
        (do ((i imaxall (1- i))           ;backwards loop-index variable
             (nextobliqua #f nextobliqua))  ; flag to indicate we are inside an obliqua
            ((< i 0))

          (let* (
                 (atfirst  (= i 0))
                 (atpenult (= i (- imax 1)))
                 (atlast   (>= i imax))
                 (atverylast (= i imaxall))
                 (inmiddle (and (> i 0) (< i imax)))

                 (previous  (if atfirst #f (list-ref notes (- i 1))))
                 (prevval   (if atfirst #f (list-ref vals (- i 1))))
                 (prevpos   (if atfirst firstpos (list-ref ypositions (- i 1))))
                 (prevflag  (if atfirst #f (list-ref flags (- i 1))))

                 (this      (list-ref notes i))
                 (thisval   (list-ref vals i))
                 (thispos   (list-ref ypositions i))
                 (thisflag  (list-ref flags i))

                 (next      (if atverylast #f (list-ref notes (+ i 1))))
                 (nextval   (if atverylast #f (list-ref vals (+ i 1))))
                 (nextpos   (if atverylast #f (list-ref ypositions (+ i 1))))
                 (nextflag  (if atverylast #f (list-ref flags (+ i 1))))

                 (descending (if atverylast (< thispos prevpos) (< nextpos thispos)))
                 (descended  (if atfirst descending (< thispos prevpos))) 

                 (singlebr (and atfirst atlast (eq? thisval 'br)))
                 (singlelg (and atfirst atlast (eq? thisval 'lg)))

                 (thiscolor  (or (assq-ref thisflag 'color) basecolor))
                 (currens    (assq-ref thisflag 'currens))

                 (obliquaB  (and (not atfirst)
                            (or
                              (and atlast                      ; obligatory sine-perf obliqua
                                   descended
                                   (not (and (= i 1)(eq? prevval 'sb)))
                                   (eq? thisval 'br))
                              (assq-ref prevflag 'obliqua)     ; manually set obliqua
                              (and (assq-ref nextflag 'pes)    ; 
                                   descended
                                   (not descending)
                                   (not (assq-ref prevflag 'noobliqua))
                                   (not (and (>= i 2) (assq-ref (list-ref flags (- i 2)) 'obliqua))))
                              (and (= i 1)                      ; try to use obliqua in descending c.opp.prop.
                                   descending
                                   (eq? thisval 'sb)
                                   (eq? prevval 'sb)
                                   (not (assq-ref thisflag 'obliqua))
                                   (not (assq-ref prevflag 'noobliqua))))))
                 (obliquaA   nextobliqua)

                 (pesbottom (or
                              (and atpenult                     ;automatic fold-in pes in ascending cum-perf
                                   foldpes                      ;(old style)
                                   (not descending) 
                                   (not obliquaB)
                                   (eq? nextval 'lg)
                                   (not (eq? thisval 'sb))
                                   (not (assq-ref nextflag 'plica))
                                   (not (assq-ref nextflag 'nopes)))
                              (and (not descending)            ;manually tweaked fold-in pes 
                                   (not obliquaB)
                                   (not (eq? thisval 'sb))
                                   (assq-ref nextflag 'pes))))
                 (pestop    (or
                              (and atlast                      ;automatic fold-in pes
                                   (not atfirst)
                                   foldpes 
                                   (not descended) 
                                   (eq? thisval 'lg)
                                   (not (eq? prevval 'sb))
                                   (not (assq-ref thisflag 'plica))
                                   (not (assq-ref thisflag 'nopes)))
                              (and (not descended)             ;manually tweaked fold-in pes
                                   (assq-ref thisflag 'pes))))
                 (plica      (and atlast (assq-ref thisflag 'plica) (< imax imaxall)))
                 (skipplica  (and atverylast (assq-ref prevflag 'plica)))
                 (opposita   (and atfirst 
                                  (< i imax)
                                  (eq? thisval 'sb)))
                 (online     (= 0 (modulo (inexact->exact (* 2 thispos)) 2)))
                 (dotatside  (lambda (x) (cons (+ x 0.3) 0)))
                 (dotabove   (cons 0 (if online 0.8 0.8)))
                 (processdot (lambda (flag offset)
                               (if (assq-ref flag 'punctum)
                                 (let ((y (cdr offset)))
                                   (while 
                                     (< linewidth (abs (- y (inexact->exact y))))
                                     (set! y (+ y 0.05)))
                                   (add-postscript
                                     (format #f "gsave newpath ~a ~a ~a 0 360 arc fill grestore " 
                                       (car offset) y (* 1.3 linewidth)))))))
                )

             ;; connector lines and relative y positioning
             (if atverylast 
               (shift-postscript 0 (- thispos ymiddle))
               (shift-postscript 0 (- thispos nextpos)))
             (if (or inmiddle atfirst)
               (let ((ydiff (- nextpos thispos)))
                 (if (= ydiff 0)
                   (shift-postscript -0.2 0)
                   (if (and (not obliquaA) (not (assq-ref nextflag 'currens)))
                     (add-postscript (ps_virga 0 ydiff thiscolor))))
                 ))

             ;; longa and plica strokes (virgae)
             (cond
               (plica
                 (let ((plicadiff (- (posref (+ i 1)) (posref i))))
                   (if (!= 0 plicadiff)
                     (let* ((plicadir  (if (= plicadiff 0) 1 (/ plicadiff (abs plicadiff))))
                            (shortplica 0.7)
                            (longplica  1.5)
                            (noplica    0)
                            (alternate  (assq-ref thisflag 'altvirga)))
                       (cond
                         (pestop
                           ;use additional plica note on the side; early-1200s style; Apel 248f.
                           (begin
                             (shift-postscript (* 0.8 notewidth) 0)
                             (add-postscript 
                               (ps_notehead 'pes thiscolor (* plicadir -0.8 notewidth) (* plicadir -0.8 noteheight)))
                             (add-postscript (ps_virga (* -0.4 notewidth) (* plicadir longplica) thiscolor))
                             (add-postscript (ps_virga (*  0.4 notewidth) (* plicadir longplica) thiscolor)))
                             (shift-postscript (* -0.8 notewidth) 0)
                             )
                         ((and atfirst atlast)
                           ;standalone nota plicata; Ars antiqua / Ars nova style; Apel 334
                           (let* ((leftplica (* plicadir
                                    (if (eq? thisval 'lg)
                                      (if (and (= plicadir 1) alternate) noplica shortplica)
                                      (if alternate shortplica longplica))))
                                  (rightplica (* plicadir 
                                    (if (eq? thisval 'lg) 
                                      longplica  
                                      shortplica))))
                              (add-postscript (ps_virga (* (- linewidth 1) notewidth) leftplica thiscolor))
                              (if (!= rightplica 0)
                                (add-postscript (ps_virga 0 rightplica thiscolor)))))
                         (else
                           ;normal ligatura plicata; Apel 248f.
                           (add-postscript (ps_virga 0 (* plicadir longplica) thiscolor))))))))

               ; alternative longa stroke of initial ascending
               ((and 
                  atfirst
                  (not descending)
                  (not obliquaA)
                  (eq? thisval 'lg)
                  (assq-ref thisflag 'altvirga))
                 (add-postscript (ps_virga (- linewidth notewidth) -2.2 thiscolor)))

               ; normal longa/maxima stroke
               ((or
                 (eq? thisval 'mx)
                 singlelg
                 (and 
                   (eq? thisval 'lg)
                   (not (assq-ref thisflag 'obliqua))
                   (or
                     (and atlast (not pestop) (not descended))
                     (and atfirst (not descending))
                     inmiddle)))
                   (let* ((usevirga (not (and (eq? thisval 'mx) (assq-ref thisflag 'novirga))))
                          (virga_down (not (assq-ref thisflag 'altvirga)))
                          (ydiff (cond
                                   ((and usevirga virga_down descending) (- nextpos thispos))
                                   ((and usevirga (not virga_down) (not descending)) (- nextpos thispos))
                                   (else 0)))
                          (ylength (+ ydiff (if virga_down -2.2 2.2)))
                          )
                     (if usevirga
                       (add-postscript (ps_virga 0 ylength thiscolor)))))

               ; exceptional forms: non-paired semibreve; Apel 100
               ((and 
                 (= i 1)
                 (eq? prevval 'sb) 
                 (not (eq? thisval 'sb)))
                 (let ((virga (if (eq? thisval 'lg) -1.5 1.5)))
                   (add-postscript (ps_virga 0 virga thiscolor))))

               ; end of virga condition block
               )


             ;; main note shapes
             (cond 
               (skipplica i)

               (currens
                 (begin
                   (shift-postscript (* -0.1 notewidth) 0)
                   (add-postscript (ps_notehead 'rhombus thiscolor notewidth (* 1.3 notewidth)))
                   (shift-postscript (* -0.7 notewidth) 0)))

               (obliquaA
                 (let* ((ydiff (- nextpos thispos))
                        (adiff (abs ydiff))
                        (width (obliqua-width adiff)))
                   (set! thisflag (assq-set! thisflag 'obliqua #t))
                   (list-set! flags i thisflag)
                   (add-postscript (ps_notehead 'obliqua thiscolor width ydiff))
                   (processdot thisflag (cons 0.3 (+ 0.8 (* 0.3 (/ ydiff width)))))
                   (set! nextobliqua #f)))

               (obliquaB
                 (let* ((ydiff (- thispos prevpos))
                        (adiff (abs ydiff))
                        (width (obliqua-width adiff)))
                   (shift-postscript linewidth 0)
                   (add-postscript (ps_notehead 'obliqua thiscolor (* -1 width) (* -1 ydiff)))
                   (if atverylast
                      (processdot thisflag (dotatside 0))
                      (processdot thisflag (cons -0.3 (+ 0.8 (* 0.3 (/ ydiff width))))))
                   (shift-postscript (- 0 width) 0)
                   (set! nextobliqua #t)))

               (pesbottom
                    (let* ((ycorr (if (< (- nextpos thispos) 1) -0.15 0)))
                      (shift-postscript (- linewidth (* 0.5 notewidth)) ycorr)
                      (add-postscript (ps_notehead 
                                      'pes thiscolor 
                                      (* -1 notewidth) 
                                      (* -1 noteheight)))
                      (processdot thisflag (dotatside (* 0.5 notewidth)))
                      (shift-postscript (* -0.5 notewidth) (- 0 ycorr))))

               (pestop
                    (let* ((ycorr (if (< (- thispos prevpos) 1) 0.15 0)))
                      (set! thisflag (assq-set! thisflag 'pes #t))
                      (list-set! flags i thisflag)
                      (shift-postscript (- linewidth (* 0.5 notewidth)) ycorr)
                      (add-postscript (ps_notehead 'pes thiscolor notewidth noteheight))
                     (if atverylast
                        (processdot thisflag (dotatside (* 0.5 notewidth)))
                        (processdot thisflag dotabove))
                      (shift-postscript (- (* 0.5 notewidth) linewidth) (- 0 ycorr))
                      ))

               (else ; normal quadrata shape
                 (let* ((ismax (eq? thisval 'mx))
                        (w (if ismax (* 2.0 notewidth) notewidth)))
                   (shift-postscript (- linewidth (* 0.5 w)) 0)
                   (add-postscript (ps_notehead 'quadrata thiscolor w noteheight))
                   (if atverylast
                      (processdot thisflag (dotatside (* 0.5 w)))
                      (processdot thisflag dotabove))
                   (shift-postscript (* -0.5 w) 0))))
               ;; end of notehead condition block

            ;; initial strokes
            (cond
              ;; cum-opp-prop.: use initial upward stroke
              (opposita
                (add-postscript (ps_virga 0 2.2 thiscolor)))
              ((and 
                 atfirst
                 (not atlast)
                 descending
                 (eq? thisval 'br)
                )
                ;;descending cum proprietate: use initial downwards stroke
                (add-postscript (ps_virga 0 -2.2 thiscolor)))
              ((and
                 atfirst
                 (not atlast)
                 (not descending)
                 (eq? nextval 'br)
                 obliquaA
                 (or (and (eq? thisval 'lg) (assq-ref thisflag 'altvirga))
                     (and (eq? thisval 'br) (not (assq-ref thisflag 'novirga)))))
                 ;;rare ascending obliques, cf. Apel 1962: 97
                 (add-postscript (ps_virga 0 -2.2 thiscolor)))
              )

             ))  ; end of note loop



        (set! postscript 

          (string-append
            (format #f "gsave currentpoint translate ~a 0 moveto currentpoint translate ~a setlinewidth 1 setlinecap " rightX linewidth)
            postscript
            " grestore"))
        (set! mystencil 
          (ly:make-stencil 
            (list 'embedded-ps postscript)
            (cons curX rightX)
            (cons 0 0)))

        ;; add accidentals
        (let ((accstencil empty-stencil)
              (accqueue   (ly:context-property context 'mensural_accidentals_queue )))
          (if (eq? accqueue #f) (set! accqueue '()))
          (map (lambda (entry)
            (set! accstencil 
              (ly:stencil-add 
                accstencil 
                (ly:stencil-translate-axis (cdr entry) (- (car entry) ymiddle) 1))))
            accqueue)
          (if (> (length accqueue) 0)
            (set! mystencil 
              (ly:stencil-combine-at-edge 
                mystencil 0 -1 accstencil 0.2)))
          (ly:context-set-property! context 'mensural_accidentals_queue '()))

        ;; set the middle note's stencil to our custom stencil;
        ;; all other stencils to zero
        (do ((i 0 (1+ i)))
            ((> i imaxall))
          (let* ((thisnote (list-ref notes i)))
            ;(begin
            ;  (ly:context-set-property!
            ;    context
            ;    'mensuralNotesQueue
            ;    (append (ly:context-property context 'mensuralNotesQueue) (list thisnote)))
            ;    )
            (if (= i imiddle)
              (ly:grob-set-property! thisnote 'stencil mystencil)
              (ly:grob-set-property! thisnote 'stencil empty-stencil))))

        ;reset queue 
        (ly:context-set-property! 
          (ly:context-property-where-defined context 'mensural_ligature_queue)
          'mensural_ligature_queue 
          #f)))))



%% ==============================================
%  NOTEHEAD CODE
%% ==============================================
%% ==============================================
%  Top-level note shape functions
%% ==============================================
#(define-public (make-mensural-note-stencil
                  notehead-type   ; one of 'quadrata, 'rhombus, 'obliqua, 'pes
                  color-type      ; one of 'black, 'white, 'halfblack, 'blackhollow, 
                                  ;        'red, 'halfred, 'redhollow
                                  ;        'gray, 'halfgray 
                  width height    ; desired dimensions, measured in staff space
                  . additions)    ; optional list of keys defined in mensural_flags below
                                  ; e.g. 'stem_above, 'flag_above_right etc.
   (let* (
          (postscript
            (string-append
              "gsave currentpoint translate "
              (format #f "~a 0 translate " (* 0.5 width))
              (ps_notehead notehead-type color-type width height)
              (string-concatenate (map (lambda (x) (ps_flag x color-type)) additions))
              "grestore ")))
      (ly:make-stencil
        (list
          'embedded-ps
          postscript)
        (cons -0.0 width)
        (cons -0.6 0.6))))

#(define-public (ps_notehead head-type color width height)
  (let* ((contours  (assq-ref mensural_notehead_contour_funcs head-type))
         (outerfunc (list-ref contours 0))
         (innerfunc (list-ref contours (case color ((halfblack halfwhite halfred halfgray halfhollow) 2) (else 1))))
         (outerps (outerfunc width height))
         (innerps (innerfunc width height))
         )
    
    (case color
      ((gray halfgray)
        (string-append
          "gsave newpath "
          outerps
          "gsave "
          innerps
          "0 setgray eofill grestore newpath "
          innerps
          "0.7 setgray fill grestore "))
      ((halfred)
        (string-append
          "gsave newpath "
          outerps
          "gsave "
          innerps
          "0 setgray eofill grestore newpath "
          innerps
          "0.7 0 0 setrgbcolor fill grestore "))
      ((red)
        (string-append
          "gsave newpath "
          outerps
          "0.7 0 0 setrgbcolor fill grestore "))
      ((halfblack)
        (string-append
          "gsave newpath "
          outerps
          ;; turn the inner shape around so that the left half will be white and the right half black
          "[-1 0 0 1 0 0] concat "
          innerps
          "0 setgray eofill grestore "))
      ((white blackhollow halfwhite hollow halfhollow)
        (string-append
          "gsave newpath "
          outerps
          innerps
          "0 setgray eofill grestore "))
      ((redhollow)
        (string-append
          "gsave newpath "
          outerps
          innerps
          "0.7 0 0 setrgbcolor eofill grestore "))
      (else
        (string-append
          "gsave newpath "
          outerps
          (if (eq? head-type 'obliqua) "closepath " "")
          "0 setgray fill grestore "))
      )))

#(define-public (ps_flag type color)
   (let* ((postscript (if (string? type) type (assq-ref mensural_flags type)))
          (ps_linestyle "1 setlinecap 0.11 setlinewidth ")
          (ps_color 
            (case color
              ((red redhollow)
               "0.7 0 0 setrgbcolor ")
              (else
               "0 setgray "))))
    (string-append ps_linestyle ps_color postscript)))


%% ====================================================
%  Rests
%% ====================================================
%% We need to fix Lilypond's buggy treatment of large 
%% mensural rests. Lilypond thinks a single three-space
%% line is a Mx rest, when in reality it's a perfect
%% Lg rest. A Mx rest is a combination of two or three
%% vertical lines, which may be either two-space or 
%% three-space long.
%% ====================================================
#(define (large_rest_stencil value modus maximodus imperfect)
  (let* ((lines (cond 
                 ((eq? value -2) 1)
                 ((or imperfect (eq? maximodus #f)) 2)
                 (else 3)))
         (length (cond
                  ((and (eq? value -2) imperfect) 2)
                  ((eq? modus #f) 2)
                  (else 3)))
         (dist   0.6)
         (width  0.3)
         (prefix (format #f "gsave currentpoint translate newpath ~a setlinewidth 0 setlinecap 0 0 moveto " width))
         (psA    (format #f "0 ~a rlineto " length))
         (psB    (format #f "~a ~a rmoveto " dist (- 0 length)))
         (ps     (string-append prefix psA))
         (xmax   (+ (* dist (- lines 1)) (* 0.5 width)))
        )
    (do ((i 2 (1+ i)))
        ((> i lines))
        (set! ps (string-append ps psB psA)))
    (set! ps (string-append ps "stroke grestore "))
    (ly:make-stencil (list 'embedded-ps ps) (cons (* -0.5 width) xmax) (cons -0.0 length))))



% ======================================================
% control the vertical position of rest signs
% ======================================================
pausa = #(define-music-function (parser location position music) (number? ly:music?)
  (let* ((fixed (if (>= position 1) #t #f))
         (y     (if fixed position 2)))
    (music-map
      (lambda (event)
        (if (memq 'rest-event (ly:music-property event 'types))
          (begin
            (set-tweak! event 'Y-offset (- y 3) #f)
            (if (not fixed)
              (set! y (if (<= y 1) 2 (- y 1))))))
        event)
      music)
    music))



%% ==================================================
%  Plica function 
%  (lplica: to be called from within a ligature)
%  takes a pair of notes, treats the second as a kind
%  of grace note subtracting its duration from the first,
%  and marks the first with the 'plica tag for later 
%  formatting.
%% ==================================================
lplica = #(define-music-function (parser location noteA noteB) (ly:music? ly:music?)
 (let* ((neA (car (ly:music-property noteA 'elements)))
        (neB (car (ly:music-property noteB 'elements)))
        (durA (ly:music-property neA 'duration))
        (durB (ly:music-property neB 'duration))
        (momA (ly:music-length noteA))
        (momB (ly:music-length noteB))
        (numA (ly:moment-main-numerator momA))
        (denA (ly:moment-main-denominator momA))
        (numB (ly:moment-main-numerator momB))
        (denB (ly:moment-main-denominator momB))
        (numC (if (= denA denB)
                  (- numA numB)
                  (- (* numA denB) (* numB denA))))
        (denC (if (= denA denB) 
                  denA
                  (* denA denB)))
        (numF (* numC denA))
        (denF (* denC numA)))
    (set-mensural-flag! neA 'plica #t)
    (make-music 'SequentialMusic
      'elements (list 
      (ly:music-compress noteA (ly:make-moment numF denF))
      noteB))))

plica = #(define-music-function (parser location noteA noteB) (ly:music? ly:music?)
#{
  \ligatura { \lplica $noteA $noteB }
#})
   
%% ===================================================
%  postscript definitions for note shape primitives
%% ===================================================

%% ===================================================
%  diagonal flexa shape, ligatures
%% ===================================================
#(define (ps_obliqua_outer_side width ydiff)
  (begin
    ;(display (format #f "width: ~a; ydiff: ~a\n" width ydiff))
    (format #f
"0 ~a translate
~a 1 scale
[1 ~a 0 1 0 0] concat
1.05 -0.3 moveto
0 -0.3 0 -0.5 0 -0.3 curveto
0 0.25 lineto
0 0.45 0 0.45 1.05 0.45 curveto " 
    (* -0.2 (/ ydiff (- (abs width) 0.4)))
    (/ width 2.0)
    (* (/ ydiff 2.0) (/ (abs width) (- (abs width) 0.4)))
    )))

#(define (ps_obliqua_inner_side width ydiff)
  (format #f
  "1.05 0.33 moveto
~a 0.33 ~:*~a 0.33 ~:*~a 0.15 curveto
 ~:*~a -0.18 lineto
 ~:*~a -0.3 ~:*~a -0.18 1.05 -0.18 curveto "
   (* 0.15 (/ 1 (abs width)))
  ))

%% ===================================================
%  rhombus shapes (semibreves and below)
%% ===================================================
#(define (ps_rhombus_outer_side width height)
  (format #f
"~a ~a scale
-0.1 0.4 moveto
0 0.5 0 0.5 0.1 0.4 curveto
0.45 0.05 lineto
0.5 0 0.5 0 0.45 -0.05 curveto
0.1 -0.4 lineto
0 -0.5 0 -0.5 -0.1 -0.4 curveto
-0.45 -0.05 lineto
-0.5 0 -0.5 0 -0.45 0.05 curveto
closepath "
    width
    height))

#(define (ps_rhombus_inner_side width height)
"[0.7 -0.1 -0.1 0.7 0 0] concat
-0.1 0.4 moveto
0 0.5 0 0.5 0.1 0.4 curveto
0.45 0.05 lineto
0.5 0 0.5 0 0.45 -0.05 curveto
0.1 -0.4 lineto
0 -0.5 0 -0.5 -0.1 -0.4 curveto
-0.45 -0.05 lineto
-0.5 0 -0.5 0 -0.45 0.05 curveto
closepath ")

#(define (ps_rhombus_half_inner_side width height)
"[0.7 -0.1 -0.1 0.7 0 0] concat 
0 0.5 moveto
0 -0.5 lineto
-0.5 0 lineto
0 0.5 lineto closepath ")    

%% ===================================================
%  quadrata shapes (brevis and above)
%% ===================================================
#(define (ps_quadrata_outer_side width height)
  (format #f
"~a ~a scale 
-0.5 0.45 moveto
-0.5 0.48 -0.48 0.5 -0.45 0.5 curveto
-0.4 0.5 -0.4 0.45 -0.25 0.45 curveto
0.25 0.45 lineto
0.4 0.45 0.4 0.5 0.45 0.5 curveto
0.48 0.5 0.5 0.48 0.5 0.45 curveto
0.5 -0.45 lineto
0.5 -0.48 0.48 -0.5 0.45 -0.5 curveto
0.4 -0.5 0.4 -0.45 0.3 -0.45 curveto
-0.3 -0.45 lineto
-0.4 -0.45 -0.4 -0.5 -0.45 -0.5 curveto
-0.48 -0.5 -0.5 -0.48 -0.5 -0.45 curveto
closepath " width height))

#(define (ps_quadrata_inner_side width height) 
  (format #f
"~a ~a scale 
-0.5 0.4 moveto
-0.5 0.46 -0.46 0.5 -0.4 0.5 curveto
0.4 0.5 lineto
0.46 0.5 0.5 0.46 0.5 0.4 curveto
0.5 -0.4 lineto
0.5 -0.46 0.46 -0.5 0.4 -0.5 curveto
-0.4 -0.5 lineto
-0.46 -0.5 -0.5 -0.46 -0.5 -0.4 curveto
closepath " 
    (/ (- width (* 0.4 height)) width)
    0.65))

#(define (ps_quadrata_half_inner_side width height)
  (format #f
"~a ~a scale 
0.5 0.4 moveto
0.5 0.46 0.46 0.5 0.4 0.5 curveto
0 0.5 lineto
0 -0.5 lineto
0.4 -0.5 lineto
0.46 -0.5 0.5 -0.46 0.5 -0.4 curveto
closepath " 
    (/ (- width (* 0.3 height)) width)
    0.76))


%% ===================================================
%  pes shapes (slightly rounded quadrata to be used
%  in ligatures)
%% ===================================================
#(define (ps_pes_outer_side width height . diffs)
  (let* ((yshift (if (> (length diffs) 0) (car diffs) 0)))
    (format #f
"0 ~a translate
~a ~a scale
-0.5 0.35 moveto
-0.5 0.45 -0.3 0.45 0 0.45 curveto
0.3 0.45 0.5 0.45 0.5 0.35 curveto
0.5 -0.45 lineto
0.5 -0.55 0.4 -0.45 0 -0.45 curveto
-0.4 -0.45 -0.5 -0.55 -0.5 -0.45 curveto
closepath " yshift width height)))

#(define (ps_pes_inner_side width height)
  (format #f
  "0 0.31 moveto
~a 0.31 ~:*~a 0.42 ~:*~a 0 curveto
~:*~a -0.42 ~:*~a -0.31 0 -0.31 curveto
~a -0.31 ~:*~a -0.42 ~:*~a 0 curveto
~:*~a 0.42 ~:*~a 0.31 0 0.31 curveto closepath "
  (-  0.5 (/ 0.15 (abs width)))
  (+ -0.5 (/ 0.15 (abs width)))))

#(define (ps_pes_half_inner_side width height)
  (format #f
  "0 0.31 moveto
~a 0.31 ~:*~a 0.42 ~:*~a 0 curveto
~:*~a -0.42 ~:*~a -0.31 0 -0.31 curveto closepath "
  (-  0.5 (/ 0.15 (abs width)))
  (+ -0.5 (/ 0.15 (abs width)))))


%% ==============================================
%  List of functions to combine into note shapes
%% ==============================================
#(define mensural_notehead_contour_funcs
  (list
    (cons 'quadrata 
       (list ps_quadrata_outer_side 
        ps_quadrata_inner_side 
        ps_quadrata_half_inner_side))
    (cons 'rhombus 
       (list ps_rhombus_outer_side
        ps_rhombus_inner_side
        ps_rhombus_half_inner_side))
    (cons 'pes 
       (list ps_pes_outer_side 
        ps_pes_inner_side
        ps_pes_half_inner_side))
    (cons 'obliqua 
      (list ps_obliqua_outer_side
       ps_obliqua_inner_side
       ps_obliqua_inner_side))
    ))

%% =============================================
%  List of available stem/flag additions
%% =============================================

#(define (ps_virga x y color)
  (let* ((realx (+ x (if (<= x 0.0) 0.055 -0.055)))
         (usered (if (or (eq? color 'red) (eq? color 'redhollow)) #t #f)))
   (string-append
     (if usered "gsave 0.7 0 0 setrgbcolor " "")
     (format #f "newpath ~a 0 moveto 0 ~a rlineto stroke " realx y)
     (if usered "grestore " ""))))

#(define mensural_flags
 '(
  (stem_above .
   "newpath 0 0.51 moveto 0 1.8 rlineto stroke ")

  (stem_below .
   "newpath 0 -0.51 moveto 0 -1.4 rlineto stroke ")

  (flag_above_right .
    "newpath 0 0.51 moveto
    0 1.7 rlineto stroke
    newpath 0 2.26 moveto
    0.019 -0.05 0.073 -0.077 0.154 -0.126 rcurveto
    0.324 -0.192 0.387 -0.356 0.45 -0.7 rcurveto
    -0.06 0 rlineto
    -0.138 0.5 -0.49 0.46 -0.55 0.66 rcurveto
    closepath fill ")

  (double_flag_above_right .
    "newpath 0 0.51 moveto
    0 1.7 rlineto stroke
    newpath 0 2.26 moveto
    0.019 -0.05 0.073 -0.077 0.154 -0.126 rcurveto
    0.324 -0.192 0.387 -0.356 0.45 -0.7 rcurveto
    -0.06 0 rlineto
    -0.138 0.5 -0.49 0.46 -0.55 0.66 rcurveto
    closepath fill 
    newpath 0 1.75 moveto
    0.019 -0.05 0.073 -0.077 0.154 -0.126 rcurveto
    0.324 -0.192 0.387 -0.356 0.45 -0.7 rcurveto
    -0.06 0 rlineto
    -0.138 0.5 -0.49 0.46 -0.55 0.66 rcurveto
    closepath fill ")

  (triple_flag_above_right .
    "newpath 0 0.51 moveto
    0 1.7 rlineto stroke
    newpath 0 2.26 moveto
    0.019 -0.05 0.073 -0.077 0.154 -0.126 rcurveto
    0.324 -0.192 0.387 -0.356 0.45 -0.7 rcurveto
    -0.06 0 rlineto
    -0.138 0.5 -0.49 0.46 -0.55 0.66 rcurveto
    closepath fill 
    newpath 0 1.75 moveto
    0.019 -0.05 0.073 -0.077 0.154 -0.126 rcurveto
    0.324 -0.192 0.387 -0.356 0.45 -0.7 rcurveto
    -0.06 0 rlineto
    -0.138 0.5 -0.49 0.46 -0.55 0.66 rcurveto
    closepath fill 
    newpath 0 1.24 moveto
    0.019 -0.05 0.073 -0.077 0.154 -0.126 rcurveto
    0.324 -0.192 0.387 -0.356 0.45 -0.7 rcurveto
    -0.06 0 rlineto
    -0.138 0.5 -0.49 0.46 -0.55 0.66 rcurveto
    closepath fill ")

  (flag_above_left .
    "newpath 0 0.51 moveto
    0 1.7 rlineto stroke
    newpath 0 2.26 moveto
    -0.019 -0.05 -0.073 -0.077 -0.154 -0.126 rcurveto
    -0.324 -0.192 -0.387 -0.356 -0.45 -0.7 rcurveto
    0.06 0 rlineto
    0.138 0.5 0.49 0.46 0.55 0.66 rcurveto
    closepath fill ")

  (flag_below right .
    "newpath 0 -0.51 moveto
    0 -1.4 rlineto stroke
    newpath 0 -1.96 moveto
    0.019 0.05 0.073 0.077 0.154 0.126 rcurveto
    0.324 0.192 0.387 0.356 0.45 0.7 rcurveto
    -0.06 0 rlineto
    -0.138 -0.5 -0.49 -0.46 -0.55 -0.66 rcurveto
    closepath fill ")

  (flag_below left .
    "newpath 0 -0.51 moveto
    0 -1.4 rlineto stroke
    newpath 0 -1.96 moveto
    -0.019 0.05 -0.073 0.077 -0.154 0.126 rcurveto
    -0.324 0.192 -0.387 0.356 -0.45 0.7 rcurveto
    0.06 0 rlineto
    0.138 -0.5 0.49 -0.46 0.55 -0.66 rcurveto
    closepath fill ")

  (vg_diagonal . "-0.33 0 moveto -0.558 -0.86 rlineto stroke ")

  (dovetail_above . "0.45 1.19 moveto -0.45 -0.65 rlineto -0.45 0.65 rlineto stroke ")

  (pigtail_above_right . 
    "newpath 0 0.51 moveto
    0.0733 0 rlineto
    0 0.7263 rlineto 
    0.26921 0.1805 0.4441 0.3356 0.44088 0.5202 rcurveto
    -0.001 0.07 -0.0409 0.131 -0.10692 0.1443 rcurveto
    -0.2204 0.044 -0.40347 -0.2512 -0.40614 -0.4129 rcurveto
    0 -0.2422 rlineto
    -0.10074 -0.063 -0.20675 -0.1174 -0.31864 -0.1686 rcurveto
    -0.004 -0.013 0.0309 -0.026 0.06 -0.019 rcurveto
    0.0658 0.023 0.15663 0.07 0.25761 0.1271 rcurveto
    0 -0.6775 rlineto
    closepath
    0.089 0.7869 rmoveto
    0.004 0.1896 rlineto 
    -0.006 0.1884 0.23085 0.3833 0.28863 0.2504 rcurveto
    0.0653 -0.1501 -0.0975 -0.2895 -0.29263 -0.44 rcurveto 
    closepath fill ")

  (dovetail_below  .
    "newpath 0 -0.51 moveto
    0 -0.6 rlineto
    -0.45 -0.7 rmoveto
     0.45  0.7 rlineto
     0.45 -0.7 rlineto
    stroke ")

  (fishhook_below_right . 
    "newpath 0.0 -0.5 moveto
    0 -0.8 rlineto
    0 -0.55 0.7 -0.55 0.7 0 rcurveto
    stroke ")

  (fishhook_below_left . 
    "newpath 0.0. -0.5 moveto
    0 -0.8 rlineto
    0 -0.55 -0.7 -0.55 -0.7 0 rcurveto
    stroke ")

  (pigtail_below_right .
    "newpath -0.05 -0.5 moveto    
0.0037844 0.0576394 0.0964026 0.0612589 0.09375 0 rcurveto
0 -0.65624995 rlineto
0.10524882 0.069477 0.44311832 0.017572 0.4375 -0.28125 rcurveto
-0.002977 -0.1583168 -0.11875128 -0.2150484 -0.25 -0.21875 rcurveto
-0.0702051 -0.00198 -0.13284105 0.014921 -0.18359375 0.058593 rcurveto
-0.045778 0.039392 -0.09367625 0.1098058 -0.09765625 0.1914068 rcurveto
-3.2669e-4 0.032748 -0.0012314 0.060905 0 0.09375 rcurveto
-0.0896129 -0.075841 -0.1447283 -0.193056 -0.21875 -0.34375 rcurveto
-0.005961 -0.012135 -0.0533322 -0.030701 -0.03125 0.023437 rcurveto
0.062823 0.190669 0.10011305 0.298224 0.25 0.4140631 rcurveto
0 0.71874995 rlineto
closepath
0.19 -1.08 rmoveto
0.1122116 -0.049485 0.233678 -0.00259 0.25 0.125 rcurveto
0.009906 0.077436 -0.0676818 0.189745 -0.15625 0.21875 rcurveto
-0.0643557 0.021076 -0.1424987 0.00582 -0.19921875 -0.027344 rcurveto
0 -0.125 rlineto
0.0020787 -0.068953 0.03860102 -0.1619177 0.10546875 -0.191406 rcurveto 
closepath eofill ")
))

#(define mensural_c_clef
   (ly:make-stencil (list
     'embedded-ps
"gsave currentpoint translate newpath 
-0.35 0 moveto
1.4 1.4 scale 
0.0106 0.4047 0.0429 0.5475 0.101 0.606 rcurveto 
0.0429 0.043 0.27535 0.039 0.45998 0 rcurveto 
0.13076 -0.035 0.15309 -0.046 0.15699 -0.1878 rcurveto 
0.003 -0.1276 -0.004 -0.1808 -0.13219 -0.1701 rcurveto 
-0.10879 0 -0.42567 0.1169 -0.4519 0 rcurveto 
-0.0372 -0.1666 -0.0429 -0.319 -0.004 -0.4664 rcurveto
 0.0283 -0.1063 0.30583 -0.1063 0.43121 -0.096 rcurveto 
 0.12545 0 0.16372 0.071 0.16372 -0.1063 rcurveto 
0 -0.1807 -0.0106 -0.2374 -0.1701 -0.2445 rcurveto 
-0.18428 0 -0.33595 0 -0.435 0.1311 rcurveto 
-0.11092 0.1772 -0.11021 0.2658 -0.1134 0.4462 rcurveto
closepath fill grestore ")
   '(-0.8 . 0.8)
   '(0 . 0)))

#(define mensural_f_clef
   (ly:make-stencil (list
     'embedded-ps
"gsave currentpoint translate newpath
1.2 1.2 scale
0.1 0.46 moveto
0.24312 -0.3359 rlineto
0.018 -0.02 0.0546 -0.023 0.0724 0 rcurveto 
0.24212 0.3335 rlineto
0.008 0.01 0.005 0.017 0 0.026 rcurveto 
-0.24212 0.3335 rlineto
-0.026 0.023 -0.0506 0.024 -0.0724 0 rcurveto
-0.24345 -0.3335 rlineto
-0.003 -0.01 -0.003 -0.016 0.0003 -0.023 rcurveto
closepath fill newpath
0.33 -0.79 moveto
0.003 -0.055 0.004 -0.652 0.0007 -0.8674 rcurveto
-4.8e-4 -0.034 0.0796 -0.039 0.0796 -0.01 rcurveto
0 0.2232 0.0116 0.8065 0.0142 0.8669 rcurveto
0.24212 0.3335 rlineto
0.008 0.01 0.006 0.017 0 0.026 rcurveto
-0.24212 0.3335 rlineto
-0.0233 0.02 -0.056 0.017 -0.0724 0 rcurveto
-0.24211 -0.3335 rlineto
-0.007 -0.01 -0.006 -0.018 0 -0.026 rcurveto
closepath fill newpath
-0.5 0.7 moveto
0.0297 0.039 0.2465 -0.02 0.34237 -0.01 rcurveto 
0.0969 0.014 0.21204 0.011 0.21204 -0.073 rcurveto 
0 -0.085 -0.0196 -1.0593 -0.0478 -1.7195 rcurveto 
-0.0282 -0.6602 0.0282 -0.7617 -0.0169 -0.7617 rcurveto 
-0.0451 0 -0.0559 0.047 -0.062 0.2031 rcurveto 
-0.006 0.1657 0.01 1.0342 -0.0104 1.0845 rcurveto 
-0.0201 0.05 -0.38575 -0.023 -0.41353 0.014 rcurveto 
-0.0307 0.04 -0.0381 0.3139 4.4e-4 0.3589  rcurveto
0.0183 0.021 0.42054 -0.051 0.42515 0.035 rcurveto 
0.004 0.075 -0.003 0.4276 -0.005 0.4853 rcurveto 
-0.002 0.066 -0.4088 -0.01 -0.43227 0.021 rcurveto 
-0.0254 0.034 -0.028 0.3126 0.008 0.3599 rcurveto 
closepath fill grestore ")
   '(-1 . 1)
   '(0 . 0)))

#(define mensural_g_clef
   (ly:make-stencil (list
     'embedded-ps
"gsave currentpoint translate 1.5 1.5 scale newpath
-0.208455 0.4111 moveto
-0.189143 -0.01 -0.369817 0.034 -0.525083 0.034  rcurveto
-0.115744 0 -0.4291 -0.042 -0.426277 -0.3952  rcurveto
0 -0.3501 0.262541 -0.5025 0.4291 -0.494  rcurveto
0.143974 0.01 0.265365 0.042 0.316179 0.096  rcurveto
-0.04799 0.1581 rlineto
0 0 -0.06211 -0.2004 -0.208904 -0.1976  rcurveto
-0.175028 0 -0.268188 0.085 -0.282303 0.3952  rcurveto
-0.0056 0.1779 0.152443 0.3501 0.287949 0.3416  rcurveto
0.141151 -0.01 0.446038 -0.073 0.446038 -0.073 rcurveto
closepath fill newpath
-0.462527 0.4716 moveto
-0.06211 -0.4263 0 -0.7142 0.02258 -0.8638 rcurveto 
0.03952 -0.2823 -0.07058 -0.3896 -0.200435 -0.4037  rcurveto
-0.127036 -0.014 -0.27948 0.065 -0.27948 0.065  rcurveto
-0.112921 -0.087  rlineto
0 0 0.05928 -0.048 0.160913 -0.065  rcurveto
0.112921 -0.02 0.231488 -0.02 0.3557 0.017  rcurveto
0.177852 0.056 0.330294 0.2033 0.293595 0.463  rcurveto
-0.03388 0.2399 -0.104452 0.4601 -0.09316 0.6154  rcurveto
0.0085 0.1327 0.04517 0.2682 0.04517 0.2682  rcurveto
closepath fill newpath
-0.414536 -0.7225 moveto
0.149621 0 0.208904 0 0.333117 0.1185  rcurveto
0.135506 0.1327 0.104452 0.7312 0.06211 0.9542  rcurveto
-0.04234 0.2259 -0.127036 0.5477 -0.129859 0.6126  rcurveto
-0.01129 0.1609 0.09598 0.2089 0.155266 0.1045  rcurveto
0.110098 -0.192 0.09598 -0.4065 0.0367 -0.4969  rcurveto
-0.07622 -0.1157 -0.197612 -0.1044 -0.364171 -0.093 rcurveto 
0 -0.1073  rlineto
0.355702 -0.031 0.533552 0.2512 0.516614 0.4827  rcurveto
-0.01694 0.223 -0.09034 0.3388 -0.225842 0.3501  rcurveto
-0.138328 0.014 -0.262542 -0.2259 -0.242781 -0.415  rcurveto
0.01694 -0.3444 0.302065 -0.9711 0.146798 -1.2986  rcurveto
-0.06493 -0.1383 -0.191966 -0.1581 -0.299241 -0.175  rcurveto
closepath fill grestore ")
   '(-1.8 . 0.8)
   '(0 . 0)))

% ===========================================================
% fix mensural clefs
% \clavis #'f #3 gives f clef on 3rd line (from bottom)
% ===========================================================
clavis = #(define-music-function (parser location type line ) (symbol? number? )
  (let* ((mystencil
           (case type
             ((f) mensural_f_clef)
             ((g) mensural_g_clef)
             (else mensural_c_clef)))
         (linepos   (* 2 (- line 3)))
         (midCpos
           (case type
             ((f) (+ (* 2 (- line 3)) 4))
             ((g) (- (* 2 (- line 3)) 4))
             (else   (* 2 (- line 3))))))

     (make-music
       'ContextSpeccedMusic
       'context-type 'Staff
       'element
         (make-music
           'SequentialMusic
           'elements (list
             (make-music
               'OverrideProperty
               ;'pop-first #t
               ;'once #t
               'symbol 'Clef
               'grob-property-path (list 'stencil)
               'grob-value mystencil)
             (make-music            ; dummy setting
               'PropertySet
               'symbol 'clefGlyph
               'value "clefs.C")
             (make-music
               'PropertySet
               'symbol 'middleCClefPosition
               'value midCpos)
             (make-music
               'PropertySet
               'symbol 'clefPosition
               'value linepos)
             (make-music
               'ApplyContext
               'procedure ly:set-middle-C!)
          )))))

%% ==============================================
%  signature_stencil
%% ==============================================
#(define (signature_stencil sign)
  (cond
    ; sign is a custom stencil
    ((ly:stencil? sign)
      sign)
    ; sign is a glyph name
    ((string? sign) 
      (lambda (grob) (grob-interpret-markup grob (markup (#:musicglyph sign)))))
    ; sign is a markup expression
    ((markup? sign)
      (lambda (grob) (grob-interpret-markup grob sign)))
    ; sign is a number pair
    ((number-pair? sign)
      (lambda (grob) (grob-interpret-markup grob 
        ;; TODO: fix vertical alignment of denominator
        (markup (make-column-markup (list
          (number->string (car sign))
          (number->string (cdr sign))))))))
      
    ; sign is a rational number
    ((rational? sign)
      (let* ((num (numerator sign))
             (den (denominator sign))
             (marknum (markup (number->string num)))
             (markden (markup (number->string den))))
        (if (= 1 den)
          (lambda (grob) (grob-interpret-markup grob marknum))
          (lambda (grob) (grob-interpret-markup grob
            ;; TODO: fix vertical alignment of denominator
            (markup (make-column-markup (list marknum markden))))))))
    ; if #f has been passed in: print nothing
    (else 
      empty-stencil)))

%% ==============================================
%  Proportions
%% ==============================================
proportio = #(define-music-function (parser location prop music) (rational? ly:music?)
  (let*  ((num (numerator prop))
          (den (denominator prop))
          (mom (ly:make-moment den num))
          (stencil (signature_stencil prop))
          (sign (list
            (make-music                     ; = \override Staff.TimeSignature #'stencil = 
              'ContextSpeccedMusic
              'context-type 'Staff
              'element (
                 make-music 'OverrideProperty
                  'pop-first #t
                  'once #t
                  'symbol 'TimeSignature
                  'grob-property-path '(stencil)
                  'grob-value stencil))
            (make-music                     
              'ContextSpeccedMusic
              'context-type 'Staff
              'descend-only #t
              'element (
                make-music 
                  'ContextSpeccedMusic
                  'context-type 'Timing
                  'element 
                    (make-music
                      'PropertySet 
                      'symbol 'timeSignatureFraction
                      'value '(4 . 4))))))
        )
    (make-music
      'SequentialMusic
      'elements (append
        sign
        (list
          (ly:music-compress music mom))))))

%% =======================================================
%  check if a music event is an 'event-chord that contains
%  at least one 'rhythmic-event (i.e. a note or rest).
%% =======================================================
#(define (rhythmicchord? event)
  (let* ((types (ly:music-property event 'types))
         (elements (ly:music-property event 'elements))
         (hasrhythmicchild #f))
    (if (and (memq 'event-chord types) (list? elements))
      (map (lambda (child)
        (let ((childtypes (ly:music-property child 'types)))
          (if (memq 'rhythmic-event childtypes)
            (set! hasrhythmicchild #t))))
        elements))
    hasrhythmicchild))
    

%% ==============================================
%  mensura command
%% ==============================================
mensura = #(define-music-function (parser location vals music) (list? ly:music?)
  (let* (
          (newmusic  (ly:music-deep-copy music))
          (sign      (assq-ref vals 'sign))
          (prolatio  (assq-ref vals 'prolatio))
          (tempus    (assq-ref vals 'tempus))
          ; if neither tempus nor prolatio are explicitly set, assume an 
          ; ars-antiqua-style mensuration on modus level (1mx = 2lg = 6br)
          (usetempus (if (or (assq 'prolatio vals) (assq 'tempus vals)) #t #f))
          (modus     (if (assq 'modus vals) (assq-ref vals 'modus) (if usetempus #t #f)))
          (maximodus (assq-ref vals 'maximodus))
          (beat      (if usetempus 
                         (ly:make-moment 1 2)          ;basic beat unit is the minim (1/2)
                         (ly:make-moment 2 1)))        ;basic beat unit is the breve

          (dimin     (or (assq-ref vals 'diminutio) 1))
          (beatgroup  (if usetempus (if prolatio 3 2) 3))
          (groupcount (if usetempus (if tempus   3 2) 2))
          (measurelength (ly:moment-mul beat (ly:make-moment (* beatgroup groupcount) 1)))
          (beatlist   (if tempus 
                          (list beatgroup beatgroup beatgroup) 
                          (list beatgroup beatgroup)))
          (fakesign   (cond
                         ((and prolatio tempus (= dimin 2)) '(9 . 8))
                         ((and prolatio tempus            ) '(9 . 4))
                         ((and prolatio        (= dimin 2)) '(6 . 8))
                         (     prolatio                     '(6 . 4))
                         ((and          tempus (= dimin 2)) '(3 . 4))
                         (                     (= dimin 2)  '(2 . 2))
                         (              tempus              '(3 . 2))
                         (usetempus                         '(4 . 4))
                         (else #f))) ;dummy setting
          (glyphname (if fakesign
            (string-append 
              "timesig.mensural" 
              (number->string (car fakesign))
              (number->string (cdr fakesign)))
            ""))
          (signstencil
            (signature_stencil
              (cond 
                ; signature has been explicitly set to false: print nothing
                ((and (pair? (assq 'sign vals)) (not sign))
                  #f)
                ; use default mensuration signs
                ((and usetempus (not (assq 'sign vals)))
                  glyphname)
                (else
                  sign))))
          (unflagged_sm (assq-ref vals 'unflagged_sm))
          (basecolor  (or (assq-ref vals 'color) 'black))
          (noteglyphs 
            (list
              (cons 'mx (lambda (color)
                (or (assq-ref vals 'mx) 
                    (make-mensural-note-stencil 
                       'quadrata color 1.7 0.75 (ps_virga 0.85  -1.4 color)))))    ;mx
              (cons 'lg (lambda (color)
                (or (assq-ref vals 'lg) 
                    (make-mensural-note-stencil 
                       'quadrata color 0.85 0.75 (ps_virga 0.425 -1.4 color)))))   ;lg
              (cons 'br (lambda (color)
                (or (assq-ref vals 'br) 
                    (make-mensural-note-stencil 
                       'quadrata color 0.85 0.75 ))))                              ;br
              (cons 'sb (lambda (color) 
                (or (assq-ref vals 'sb) 
                    (make-mensural-note-stencil 
                       'rhombus  color 0.8 1.1 ))))                                ;sb
              (cons 'mn (lambda (color) 
                (or (assq-ref vals 'mn) 
                (make-mensural-note-stencil 
                       'rhombus  color 0.8 1.1 'stem_above))))                     ;mn
              (cons 'sm (lambda (color) 
                (or (assq-ref vals 'sm) 
                        (if unflagged_sm                                          ;sm
                          (make-mensural-note-stencil
                            'rhombus
                            (case color (( black ) 'blackhollow ) (( white blackhollow hollow ) 'black) (else color))
                            0.8 1.1
                            'stem_above)
                          (make-mensural-note-stencil 
                            'rhombus  
                            color 
                            0.8 1.1 
                            'flag_above_right)))))
              (cons 'fu (lambda (color) 
                (or (assq-ref vals 'fu)                                   ;fu
                        (if unflagged_sm
                          (make-mensural-note-stencil
                            'rhombus
                            (case color (( black ) 'blackhollow ) (( white blackhollow hollow ) 'black) (else color))
                            0.8 1.1
                            'flag_above_right)
                          (make-mensural-note-stencil 
                           'rhombus
                           color 
                           0.8 1.1 
                           'double_flag_above_right)))))
              (cons 'sf (lambda (color) 
                (or (assq-ref vals 'sf)                                   ;semifusa
                        (if unflagged_sm
                          (make-mensural-note-stencil
                            'rhombus
                            (case color (( black ) 'blackhollow ) (( white blackhollow hollow ) 'black) (else color))
                            0.8 1.1
                            'double_flag_above_right)
                          (make-mensural-note-stencil 
                           'rhombus
                           color 
                           0.8 1.1 
                           'triple_flag_above_right)))))
             ))
             (notesettings
               (list
                (cons 'foldpes (assq-ref vals 'foldpes))
                (cons 'color basecolor)
                (cons 'unflagged_sm unflagged_sm)))
          (notestencilfunction  (lambda (grob)
            (let* ((cause (ly:grob-property grob 'cause))
                   (duration (ly:event-property cause 'duration))
                   (durlog (ly:duration-log duration))
                   (dur (duration_log_to_note_symbol durlog))
                   (tags     (ly:grob-property grob 'mensural_tags))
                   (colortweak (get-mensural-flag cause 'color))
                   (thiscolor   (if colortweak colortweak basecolor))
                   (thisstencil #f))
              (if (assoc duration vals)
                (set! thisstencil (assoc-ref vals duration))
                (set! thisstencil ((assq-ref noteglyphs dur) thiscolor))
               )
              thisstencil)))
          (reststencilfunction (lambda (grob)
            (let* ((cause (ly:grob-property grob 'cause))
                   (duration (ly:event-property cause 'duration))
                   (durlog (ly:duration-log duration))
                   (tags     (ly:grob-property grob 'mensural_tags))
                   (imperfect (if tags (assq-ref tags 'imperfect) #f))
                  )
              (if (<= durlog -2)
                (large_rest_stencil durlog modus maximodus imperfect)
                (ly:rest::print grob)))))
          (widthfunction (lambda (grob)
            (let* ((cause (ly:grob-property grob 'cause))
                   (duration (ly:event-property cause 'duration))
                   (durlog (ly:duration-log duration)))
              (if (<= durlog -2)
                (ly:grob::stencil-width grob)
                (ly:rest::width grob)))))

          (result (list 
            (make-music
              'ContextSpeccedMusic
              'context-type 'Bottom
              'element
              (make-music
                'OverrideProperty
                'symbol 'NoteHead
                'grob-property-path (list 'mensural_glyphs)
                'grob-value notesettings))
            (make-music
              'ContextSpeccedMusic               ; = \set mensural_ligature_queue = #f
              'context-type 'Bottom
              'element
              (make-music
                'PropertySet
                'symbol 'mensural_ligature_queue
                'value #f))

            ;(make-music                         ; =   \override NoteHead #'X-extent = #widthfunction
            ;  'ContextSpeccedMusic
            ;  'context-type 'Bottom
            ;  'element
            ;  (make-music
            ;    'OverrideProperty
            ;    'pop-first #t
            ;    'symbol 'NoteHead
            ;    'grob-property-path (list 'X-extent)
            ;    'grob-value widthfunction))
            (make-music                         ; =   \override Rest #'X-extent = #widthfunction
              'ContextSpeccedMusic
              'context-type 'Bottom
              'element
              (make-music
                'OverrideProperty
                'symbol 'Rest
                'grob-property-path (list 'X-extent)
                'grob-value widthfunction))

            (make-music                         ; =   \override NoteHead #'stencil = #notestencilfunction
              'ContextSpeccedMusic
              'context-type 'Bottom
              'element
              (make-music
                'OverrideProperty
                'symbol 'NoteHead
                'grob-property-path (list 'stencil)
                'grob-value notestencilfunction))

            (make-music                         ; =   \override Rest #'stencil = #reststencilfunction
              'ContextSpeccedMusic
              'context-type 'Bottom
              'element
              (make-music
                'OverrideProperty
                'symbol 'Rest
                'grob-property-path (list 'stencil)
                'grob-value reststencilfunction))


           (make-music
              'ContextSpeccedMusic              ; = \override Rest #'style = #'mensural
              'context-type 'Bottom
              'element
              (make-music
                'OverrideProperty
                'symbol 'Rest
                'grob-property-path (list 'style)
                'grob-value 'mensural))
            (make-music                     ; = \override Staff.TimeSignature #'stencil = #signstencil
              'ContextSpeccedMusic
              'context-type 'Staff
              'element (
                 make-music 'OverrideProperty
                  'once #t
                  'symbol 'TimeSignature
                  'grob-property-path '(stencil)
                  'grob-value signstencil))
            (make-music                     
              'ContextSpeccedMusic
              'context-type 'Staff
              'descend-only #t
              'element (
                make-music 
                  'ContextSpeccedMusic
                  'context-type 'Timing
                  'element (
                    make-music
                      'SequentialMusic
                      'elements (append 
                        (if (pair? fakesign)
                          (list
                            (make-music
                              'PropertySet 
                              'symbol 'timeSignatureFraction
                              'value fakesign))
                          '())
                        (list
                          (make-music
                            'PropertySet
                            'symbol 'beatLength
                            'value beat)
                          (make-music
                            'PropertySet
                            'symbol 'measureLength
                            'value measurelength)
                          (make-music
                            'PropertySet
                            'symbol 'beatGrouping
                            'value beatlist)
                          )))))))


       )

   ;; now process the actual music, setting durations according to tempus/prolatio
   (music-map
     (lambda (event)
       (cond 
         ;;every chord of the music needs to be prefixed
         ;;with an ApplyOutput call to our mensural_processing function
         ((rhythmicchord? event)
           (begin
             ;(display "prefixing an event-chord:\n")
             (make-music          
               'SequentialMusic    
               'elements
               (list
                 (make-music
                   'ApplyOutputEvent
                   'context-type 'Bottom
                   'procedure mensural_processing)
                   event))))
         ((memq 'rhythmic-event (ly:music-property event 'types))
           (let* ((duration  (ly:music-property event 'duration))
                  (durlog    (ly:duration-log duration))
                  (dur       (duration_log_to_note_symbol durlog))
                  (dots      (ly:duration-dot-count duration))
                  (durfactor (ly:duration-factor duration))
                  (color     (or (get-mensural-flag event 'color) basecolor))
                  (newfactor '(1 . 1))
                  (fprol (if prolatio  3 2))
                  (ftemp (if tempus    3 2))
                  (fmod  (if modus     3 2))
                  (fmax  (if maximodus 3 2))
                  (multiply (lambda (pairA pairB) (cons (* (car pairA)(car pairB)) (* (cdr pairA)(cdr pairB)))))
                  (newevent '())
                 )
             ;(display (format #f "We have a rhythmic event: ~a\n" event))
             (case dur
               (( mx )  ; maxima
                 (set! newfactor (cons (* fprol ftemp fmod fmax) 16)))
               (( lg )  ; longa
                   (set! newfactor (cons (* fprol ftemp fmod) 8)))
               (( br )  ; breve
                 (set! newfactor (cons (* fprol ftemp) 4)))
               (( sb )   ; semibreve 
                 (set! newfactor (cons fprol 2))))
             (set! newfactor (multiply newfactor durfactor))
             (ly:music-set-property! 
               event 
               'duration 
               (ly:make-duration durlog dots (car newfactor) (cdr newfactor)))
          


         event))
         (else event)
         ))
     newmusic)

   ; apply proportion
   (if (!= dimin 1)
     (set! newmusic 
       (ly:music-compress newmusic
		     (ly:make-moment (denominator dimin) (numerator dimin)))))

   ; insert the time sig code together with the modified music
   (make-music 
     'SequentialMusic
     'elements (append 
      result 
      (list 
        newmusic
      )))
   ))

linea = #(define-music-function (parser location type) (string?)
#{
  \once \override Staff.BarLine #'stencil = #ly:bar-line::print
  \bar $type
#})

%% ==================================================================
%  Accidentals function: use medieval b molle and b durum,
%  "♮" = b natural or f sharp; "♭" = b flat or f natural
%% ==================================================================
accidens = #(define-music-function (parser location note) (ly:music?)
  (let ((result '())
        (notecopy (ly:music-deep-copy note)))
    (music-map
      (lambda (event)
        (if (memq 'note-event (ly:music-property event 'types))
          (let* ((pitch      (ly:music-property event 'pitch))
                 (notename   (ly:pitch-notename pitch))
                 (alteration (ly:pitch-alteration pitch)))
            (set! result
                  (make-music
                    'SequentialMusic
                    'elements (append (list 
                      (make-music 
                        'ContextSpeccedMusic
                        'context-type 'Bottom
                        'element (make-music
                          'OverrideProperty
                          'once #t
                          'symbol 'Accidental
                          'grob-property-path '(stencil)
                          'grob-value ly:accidental-interface::print))
                      (make-music
                        'ContextSpeccedMusic
                        'context-type 'Bottom
                        'element (make-music
                          'OverrideProperty
                          'once #t
                          'symbol 'Accidental
                          'grob-property-path '(glyph-name-alist)
                          'grob-value 
                            (case notename
                              ((3 0 4) '((0    . "accidentals.flat")(1/2 . "accidentals.natural")))    ; c,f,g
                              (else    '((-1/2 . "accidentals.flat")(0   . "accidentals.natural"))))   ; b,e,a,d
                       )))
                       (list note))))
            (ly:music-set-property! event 'force-accidental #t)
            )))
      notecopy)
    result ))




