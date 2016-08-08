\version "2.17.29"

% preliminary LilyJAZZ version for testing purposes

\paper { indent = #0 }

%***********************************************************************
% MAPPING ALISTS / LOOKUP TABLES
%***********************************************************************

% MAPPING ALIST: GLYPH NAME TO UNICODE CHAR NUMBER =====================
#(define jazz-map '(
    ("noteheads.s0jazz" . #xe191) 
    ("noteheads.s1jazz" . #xe192)
    ("noteheads.s2jazz" . #xe193)
    ("noteheads.s0slashjazz" . #xe19c)
    ("noteheads.s1slashjazz" . #xe19d)
    ("noteheads.s2slashjazz" . #xe19e)
    ("noteheads.s2crossjazz" . #xe1a1)
    ("flags.u3jazz" . #xe21c)
    ("flags.u4jazz" . #xe21d)
    ("flags.u5jazz" . #xe21e)
    ("flags.u6jazz" . #xe21f)
    ("flags.u7jazz" . #xe220)
    ("flags.d3jazz" . #xe221)
    ("flags.d4jazz" . #xe222)
    ("flags.d5jazz" . #xe223)
    ("flags.d6jazz" . #xe224)
    ("flags.d7jazz" . #xe225)
    ("flags.ugracejazz" . #xe226)
    ("flags.dgracejazz" . #xe227)
    ("dots.dot" . #xe131)
    ("accidentals.doublesharpjaz" . #xe126)
    ("accidentals.sharpjazz" . #xe10f)
    ("accidentals.naturaljazz" . #xe117)
    ("accidentals.flatjazz" . #xe11b)
    ("accidentals.flatflatjazz" . #xe124)
    ("accidentals.rightparenjaz" . #xe127)
    ("accidentals.leftparenjazz" . #xe128)
    ("rests.mmendjazz" . #xe0f8)
    ("rests.0jazz" . #xe100)
    ("rests.1jazz" . #xe101)
    ("rests.0ojazz" . #xe102)
    ("rests.1ojazz" . #xe103)
    ("rests.2jazz" . #xe108)
    ("rests.3jazz" . #xe10a)
    ("rests.4jazz" . #xe10b)
    ("rests.5jazz" . #xe10c)
    ("rests.6jazz" . #xe10d)
    ("rests.7jazz" . #xe10e)
    ("clefs.Fjazz" . #xe170)
    ("scripts.ufermatajazz" . #xe132)
    ("scripts.dfermatajazz" . #xe133)
    ("scripts.asteriskjazz" . #xe138)
    ("scripts.sforzatojazz" . #xe13b)
    ("scripts.esprjazz" . #xe13c)
    ("scripts.staccatojazz" . #xe13d)
    ("scripts.tenutojazz" . #xe140)
    ("scripts.uportatojazz" . #xe141)
    ("scripts.dportatojazz" . #xe142)
    ("scripts.umarcatojazz" . #xe143)
    ("scripts.dmarcatojazz" . #xe144)
    ("scripts.openjazz" . #xe145)
    ("scripts.stoppedjazz" . #xe148)
    ("scripts.trilljazz" . #xe14d)
    ("scripts.segnojazz" . #xe153)
    ("scripts.varsegnojazz" . #xe154)
    ("scripts.codajazz" . #xe155)
    ("scripts.varcodajazz" . #xe156)
    ("scripts.trill_elementjazz" . #xe15c)
    ("scripts.trilelementjazz" . #xe15f)
    ("scripts.pralljazz" . #xe160)
    ("scripts.prallpralljazz" . #xe162)
    ("clefs.F_changejazz" . #xe171)
    ("clefs.Gjazz" . #xe172)
    ("clefs.G_changejazz" . #xe173)
    ("timesig.C44jazz" . #xe178)
    ("timesig.C22jazz" . #xe179)
    ("zerojazz" . #x0030)
    ("onejazz" . #x0031)
    ("twojazz" . #x0032)
    ("threejazz" . #x0033)
    ("fourjazz" . #x0034)
    ("fivejazz" . #x0035)
    ("sixjazz" . #x0036)
    ("sevenjazz" . #x0037)
    ("eightjazz" . #x0038)
    ("ninejazz" . #x0039)
    ("Fjazz" . #x0046)
    ("fjazz" . #x0066)
    ("mjazz" . #x006d)
    ("pjazz" . #x0070)
    ("rjazz" . #x0072)
    ("sjazz" . #x0073)
    ("zjazz" . #x007a)))                            

#(define jazz-alteration-glyph-name-alist
  '((0 . "accidentals.naturaljazz")
    (-1/2 . "accidentals.flatjazz")
    (1/2 . "accidentals.sharpjazz")
    (1 . "accidentals.doublesharpjaz")
    (-1 . "accidentals.flatflatjazz")))


%***********************************************************************
%  JAZZ GLYPH ACCESS
%***********************************************************************

#(define-markup-command (jazzchar layout props charnum) (number?)
  "char for jazz characters"
    (interpret-markup layout props
      (markup (#:fontsize 5 #:override '(font-name . "LilyJAZZ") #:char charnum))))

% JAZZGLYPH: REPLACEMENT FOR MUSICGLYPH ================================
#(define-markup-command (jazzglyph layout props glyphname) (string?)
  "musicglyph replacemet for jazz (i. e. non-Feta) characters"
    (let* ((charnum (cdr (assoc glyphname  jazz-map))))
    (interpret-markup layout props
      (markup (#:fontsize 5 #:override '(font-name . "LilyJAZZ") #:char charnum)))))


%***********************************************************************
%  JAZZ CLEFS
%***********************************************************************

#(define (jazz-clef grob)
  "jazz clef stencil"
  (let* ((glyphname (string-append (ly:grob-property grob 'glyph-name) "jazz")))    
    (if (pair? (assoc glyphname jazz-map))
        (grob-interpret-markup grob (markup #:jazzglyph glyphname))
        (ly:clef::print grob))))


%***********************************************************************
% KEY SIGNATURES
%***********************************************************************

#(define (jazz-keysig grob) 
  "stencil: jazz key signature (including cancellation)" 
  (let* ((altlist (ly:grob-property grob 'alteration-alist)) 
    (c0pos (ly:grob-property grob 'c0-position)) 
    (keysig-stencil '())) 
    (for-each (lambda (alt) 
         (let* ((alteration (if (grob::has-interface grob 'key-cancellation-interface) 0 (cdr alt))) 
         (glyphname (assoc-get alteration jazz-alteration-glyph-name-alist 
"")) 
         (padding (cond 
           ((< alteration 0) 0.25)  ; any kind of flat 
           ((= alteration 0) 0.05)  ;  natural 
           ((< alteration 1) 0.1)   ; sharp (less than double sharp) 
           (else -0.4)))            ; double sharp 
         (ypos (key-signature-interface::alteration-positions alt c0pos grob)) 
         (acc-stencil (fold (lambda (y s) 
                              (ly:stencil-add 
                                (grob-interpret-markup grob 
                                  (markup #:raise (/ y 2) #:jazzglyph 
glyphname)) 
                                s)) 
                            empty-stencil 
                            ypos))) 
         (set! keysig-stencil (ly:stencil-combine-at-edge acc-stencil X 
RIGHT keysig-stencil padding)))) altlist) 
    keysig-stencil)) 


%***********************************************************************
% TIME SIGNATURE
%***********************************************************************

#(define (jazz-timesig grob)
  (let* ((style (ly:grob-property grob 'style))
         (fraction (ly:grob-property grob 'fraction))
         (glyphname (if (equal? style 'C) 
                (cond 
                   ((equal? fraction '(4 . 4)) "timesig.C44jazz")
                   ((equal? fraction '(2 . 2)) "timesig.C22jazz")
                   (else "")) "")))
    (if (equal? glyphname "")
        (ly:time-signature::print grob)
        (grob-interpret-markup grob (markup #:fontsize -5 #:jazzglyph glyphname)))))


%***********************************************************************
% NOTE HEADS
%***********************************************************************

#(define (jazz-notehead grob)
  "stencil: jazz noteheads"
  (let* ((log (ly:grob-property grob 'duration-log)))
     (set! (ly:grob-property grob 'stem-attachment) '(1.0 . 0.35))
     (grob-interpret-markup grob
      (cond 
         ((<= log 0) (markup #:jazzglyph "noteheads.s0jazz"))
         ((<= log 1) (markup #:jazzglyph "noteheads.s1jazz"))  
	 (else  (markup #:jazzglyph "noteheads.s2jazz"))))))

%***********************************************************************
% FLAGS
%***********************************************************************

#(define (jazz-flag grob)
  "stencil: jazz flags"
   (let* ((stem-grob (ly:grob-parent grob X))
          (log (ly:grob-property stem-grob 'duration-log))
          (dir (ly:grob-property stem-grob 'direction))
          (stem-width (* (ly:staff-symbol-line-thickness grob) (ly:grob-property stem-grob 'thickness)))
          (glyphname (string-append "flags." (if (> dir 0) "u" "d") (number->string log) "jazz"))
          (flag-stencil (grob-interpret-markup grob (markup #:jazzglyph glyphname)))
          (flag-pos (cons (* stem-width -1) 0))
          (stroke-style (ly:grob-property grob 'stroke-style))
          (stroke-stencil (if (equal? stroke-style "grace")
                              (if (equal? dir UP) 
                                  (make-line-stencil 0.15 -0.5 -1.6 0.75 -0.6)
                                  (make-line-stencil 0.15 -0.4 1.6 0.85 0.6))
     ;                             (grob-interpret-markup grob (markup #:jazzglyph "flags.ugracejazz"))
     ;                             (grob-interpret-markup grob (markup #:jazzglyph "flags.dgracejazz")))
                              empty-stencil)))
          (ly:stencil-translate (ly:stencil-add flag-stencil stroke-stencil) flag-pos)))


%***********************************************************************
% DOTS (DURATION)
%***********************************************************************

#(define (jazz-dots grob)
  "stencil: jazz duration dots"
  (let* ((dot-count (ly:grob-property grob 'dot-count)))
     (grob-interpret-markup grob (markup
         (if (>= dot-count 1) (markup #:translate '(0.4 . 0) #:jazzglyph "dots.dot") (markup #:null))
         (if (>= dot-count 2) (markup #:translate '(0.25 . 0) #:jazzglyph "dots.dot") (markup #:null))
         (if (>= dot-count 3) (markup #:translate '(0.25 . 0) #:jazzglyph "dots.dot") (markup #:null))
         (if (>= dot-count 4) (markup #:translate '(0.25 . 0) #:jazzglyph "dots.dot") (markup #:null))
         (if (>= dot-count 5) (markup #:translate '(0.25 . 0) #:jazzglyph "dots.dot") (markup #:null))))))         


%***********************************************************************
% ACCIDENTALS
%***********************************************************************

% ACCIDENTALS (MUSICAL CONTEXT) ========================================
#(define (jazz-accidental grob)
   "stencil: jazz accidentals in front of notes or used as suggestions"
  (let* ((alt (ly:grob-property grob 'alteration))
         (show (if (null? (ly:grob-property grob 'forced)) (if (null? (ly:grob-object grob 'tie)) #t #f ) #t )))
    (if (equal? show #t)
      (grob-interpret-markup grob (markup #:jazzglyph (assoc-get alt jazz-alteration-glyph-name-alist "")))
      (ly:accidental-interface::print grob))))

% CAUTIONARY ACCIDENTALS (MUSICAL CONTEXT) =============================
#(define (jazz-accidental-cautionary grob)
   "stencil: jazz cautionary accidentals in front of notes"
  (let* ((alt (ly:grob-property grob 'alteration)))
    (grob-interpret-markup grob
      (case alt 
         ((1) (markup #:concat (
                     #:translate '(0 . -0.05) #:jazzglyph "accidentals.leftparenjazz" 
                     #:jazzglyph "accidentals.doublesharpjaz"                         
                     #:translate '(0 . 0.05) #:jazzglyph "accidentals.rightparenjaz" )))
         ((1/2) (markup #:concat (
                     #:translate '(0 . -0.1) #:jazzglyph "accidentals.leftparenjazz" 
                     #:jazzglyph "accidentals.sharpjazz"                       
                     #:translate '(0 . 0.3) #:jazzglyph "accidentals.rightparenjaz" )))
         ((0) (markup #:concat (
                     #:translate '(0.1 . 0) #:rotate 5 #:jazzglyph "accidentals.leftparenjazz" 
                     #:jazzglyph "accidentals.naturaljazz"                          
                     #:translate '(0.05 . 0.3) #:rotate 5 #:jazzglyph "accidentals.rightparenjaz" )))
         ((-1/2) (markup #:concat (
                     #:translate '(-0.15 . 0.3) #:jazzglyph "accidentals.leftparenjazz" 
                     #:jazzglyph "accidentals.flatjazz"                                   
                     #:translate '(0 . 0.2) #:jazzglyph "accidentals.rightparenjaz" )))   ;   
         ((-1) (markup #:concat (
                     #:translate '(-0.15 . 0.5) #:jazzglyph "accidentals.leftparenjazz" 
                     #:jazzglyph "accidentals.flatflatjazz"                                  
                     #:translate '(-0.1 . 0.4) #:jazzglyph "accidentals.rightparenjaz" )))))))


%***********************************************************************
% RESTS
%***********************************************************************

#(define (jazz-rest grob)
  (let* ((duration (ly:grob-property grob 'duration-log))
         (glyphname (string-append "rests." (number->string duration) "jazz")))
      (grob-interpret-markup grob (markup #:jazzglyph glyphname))))


%***********************************************************************
% DYNAMICS
%***********************************************************************

  sfpp = #(make-dynamic-script  "sfpp" )
% re-defined because of spefilal f with horizontal bar for ligatures/grouping:
  ff = #(make-dynamic-script  "FF" )
  fff = #(make-dynamic-script  "FFF" )
  ffff = #(make-dynamic-script  "FFFF" )
  fffff = #(make-dynamic-script  "FFFFF" )

% to do: make reversible for \jazzOff
#(define-markup-command (dynamic layout props arg)
  (markup?)
  #:category font
  "Use jazz instead of feta for dynamic markup font."
  (interpret-markup layout props (markup #:fontsize 5 #:override '(font-name . "LilyJAZZ") arg)))



%***********************************************************************
% ACCENTS
%***********************************************************************

#(define (jazz-articulation grob)
  (let* ((dir (ly:grob-property grob 'direction))
           (var (ly:grob-property grob 'script-stencil))
           (glyphname (if (= dir DOWN) (car (cdr var)) (cdr (cdr var))))
           (jazzchar (assoc-get (string-append "scripts." glyphname "jazz") jazz-map 0 )))
     (if (> jazzchar 0) 
         (grob-interpret-markup grob (markup #:jazzchar jazzchar))
         (ly:script-interface::print grob))))


fermataMarkup =
#(make-music 'MultiMeasureTextEvent
             'tweaks (list
                      ;; Set the 'text based on the 'direction
                      (cons 'text (lambda (grob)
                                    (if (eq? (ly:grob-property grob 'direction) DOWN)
                                        (markup #:concat (" " #:jazzglyph "scripts.dfermatajazz"))
                                        (markup #:concat (" " #:jazzglyph "scripts.ufermatajazz")))))
                      (cons 'outside-staff-priority 40)
                      (cons 'outside-staff-padding 0)))


makeUnpurePureContainer =
        #(ly:make-unpure-pure-container
       ly:grob::stencil-height
       (lambda (grob start end) (ly:grob::stencil-height grob)))  


jazzOn = {
  \override Staff.Clef.stencil = #jazz-clef
  \override Score.BarNumber.stencil = ##f
  
  \override Staff.KeySignature.stencil = #jazz-keysig
  \override Staff.KeyCancellation.stencil = #jazz-keysig

  \override Staff.TimeSignature.stencil = #jazz-timesig
  
  \override Staff.NoteHead.stencil = #jazz-notehead  
  \override Staff.NoteHead.Y-extent = \makeUnpurePureContainer
  
  \override Staff.Stem.thickness = #2
  \override Staff.Beam.beam-thickness = #0.55
  \override Staff.Flag.stencil = #jazz-flag
  \override Staff.Flag.Y-extent = \makeUnpurePureContainer

  \override Staff.Dots.stencil = #jazz-dots
  \override Staff.Accidental.stencil = #jazz-accidental
  \override Staff.Accidental.Y-extent = \makeUnpurePureContainer
  
  \override Staff.AccidentalCautionary.stencil = #jazz-accidental-cautionary
  \override Staff.AccidentalCautionary.Y-extent = \makeUnpurePureContainer

  \override Staff.AccidentalSuggestion.stencil = #jazz-accidental
  \override Staff.AccidentalSuggestion.Y-extent = \makeUnpurePureContainer
        
  \override Staff.Script.stencil = #jazz-articulation        
  
  \override Staff.Rest.stencil = #jazz-rest
  \override Staff.Slur.thickness = #2.5
  \override Staff.Tie.thickness = #2.5
  \override Staff.BarLine.hair-thickness = #3  
  \override Score.BarNumber.font-name = #"LilyJAZZ Text"

  \override Staff.TimeSignature.font-name = #"LilyJAZZ"
  \override Staff.TimeSignature.font-size = #5
  
  \override Staff.DynamicText.font-name = #"LilyJAZZ"
  \override Staff.DynamicText.font-size = #6

  \override Score.RehearsalMark.font-name = #"LilyJAZZ Text"
  \override Score.MetronomeMark.font-name = #"LilyJAZZ Text"

  \override Staff.TextScript.font-name = #"LilyJAZZ Text"   
  \override Staff.TextScript.font-size = #1

  \override Staff.InstrumentName.font-name = #"LilyJAZZ Text"   
  
  \override Staff.TupletNumber.font-name = "LilyJAZZ Text"
  \override Staff.TupletBracket.thickness = #1.5
  
  \override Score.VoltaBracket.font-name = #"LilyJAZZ Text"
  \override Score.VoltaBracket.font-size = #0
  \override Score.VoltaBracket.thickness = #2
}

jazzOff = {
  \revert Staff.Clef.stencil
  
  \revert Staff.KeySignature.stencil
  \revert Staff.KeyCancellation.stenci
  
  \revert Staff.TimeSignature.stencil

  \revert Staff.NoteHead.stencil
  \revert Staff.NoteHead.Y-extent
  \revert Staff.Stem.thickness
  \revert Staff.Beam.beam-thickness
  \revert Staff.Flag.stencil
  \revert Staff.Dots.stencil
  \revert Staff.Accidental.stencil
  \revert Staff.AccidentalCautionary.stencil
  \revert Staff.AccidentalSuggestion.stencil
  \revert Staff.Script.stencil 
  \revert Staff.Rest.stencil
  \revert Staff.Slur.thickness 
  \revert Staff.Tie.thickness
  \revert Staff.BarLine.hair-thickness
  \revert Score.BarNumber.font-name
  \revert Staff.TimeSignature.font-name
  \revert Staff.TimeSignature.font-size
  \revert Staff.DynamicText.font-name
  \revert Staff.DynamicText.font-size
  \revert Score.RehearsalMark.font-name
  \revert Score.MetronomeMark.font-name
  \revert Staff.TextScript.font-name  
  \revert Staff.TextScript.font-size
  \revert Staff.InstrumentName.font-name   
  \revert Staff.TupletNumber.font-name
  \revert Staff.TupletBracket.thickness
  \revert Score.VoltaBracket.font-name
  \revert Score.VoltaBracket.font-size
  \revert Score.VoltaBracket.thickness
}



