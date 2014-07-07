\version "2.18.0"

#(define-public (naturalize-pitch p)
   (let ((o (ly:pitch-octave p))
         (a (* 4 (ly:pitch-alteration p)))
         ;; alteration, a, in quarter tone steps,
         ;; for historical reasons
         (n (ly:pitch-notename p)))
     (cond
      ((and (> a 1) (or (eq? n 6) (eq? n 2)))
       (set! a (- a 2))
       (set! n (+ n 1)))
      ((and (< a -1) (or (eq? n 0) (eq? n 3)))
       (set! a (+ a 2))
       (set! n (- n 1))))
     (cond
      ((> a 2) (set! a (- a 4)) (set! n (+ n 1)))
      ((< a -2) (set! a (+ a 4)) (set! n (- n 1))))
     (if (< n 0) (begin (set! o (- o 1)) (set! n (+ n 7))))
     (if (> n 6) (begin (set! o (+ o 1)) (set! n (- n 7))))
     (ly:make-pitch o n (/ a 4))))
#(define-public (naturalize music)
   (let ((es (ly:music-property music 'elements))
         (e (ly:music-property music 'element))
         (p (ly:music-property music 'pitch)))
     (if (pair? es)
         (ly:music-set-property!
          music 'elements
          (map (lambda (x) (naturalize x)) es)))
     (if (ly:music? e)
         (ly:music-set-property!
          music 'element
          (naturalize e)))
     (if (ly:pitch? p)
         (begin
          (set! p (naturalize-pitch p))
          (ly:music-set-property! music 'pitch p)))
     music))

#(define (get-option key options def)
   (let ((default (ly:assoc-get 'default options '() #f)))
     (ly:assoc-get key options (ly:assoc-get key default def #f) #f)
     ))

\registerTemplate lalily.instrument
#(define-music-function (parser location piece options)(list? list?)
   (let ((name (get-option 'name options "instrument"))
         (init-voice (get-option 'init-voice options #f))
         (clef (get-option 'clef options #f))
         (transp (get-option 'transposition options (ly:make-pitch 0 0 0)))
         (natpit (get-option 'naturalize options #f))
         (input-concert-pitch (get-option 'input-concert-pitch options #t))
         (output-concert-pitch (get-option 'output-concert-pitch options #t))
         (staff-mods (get-option 'staff-mods options #f))
         (voice-mods (get-option 'voice-mods options #f))
         (midi-instrument (get-option 'midi-instrument options #f))
         (meta (get-music-deep piece 'meta #f location))
         )
     (define (natmus mus) (if natpit (naturalize mus) mus))
     (if (string? clef)
         (set! meta (make-music 'SimultaneousMusic
                      'elements (list #{ \clef #clef #} meta)))
         )
     #{
       \new Staff = $name \with {
         $(if (ly:context-mod? staff-mods) staff-mods)
         \consists \editionEngraver $piece
         midiInstrument = #midi-instrument
       } \new Voice \with {
         $(if (ly:context-mod? voice-mods) voice-mods)
       } {
         $(if (not output-concert-pitch) #{ \transposition $transp #})
         $(cond
           ((and input-concert-pitch (not output-concert-pitch))
            (natmus #{
              \transpose $transp c' <<
                { #meta }
                { \getMusicDeep {} #(glue-symbol (list name 'global) "-") $(if (ly:music? init-voice) init-voice) \getMusic #'() }
              >>
              #}))
           ((and (not input-concert-pitch) output-concert-pitch)
            (natmus #{
              <<
                { #meta }
                \transpose c' $transp { \getMusicDeep {} #(glue-symbol (list name 'global) "-") $(if (ly:music? init-voice) init-voice) \getMusic #'() }
              >>
              #}))
           ((and (not input-concert-pitch) (not output-concert-pitch))
            #{
              <<
                \transpose c' $transp { #meta }
                { \getMusicDeep {} #(glue-symbol (list name 'global) "-") $(if (ly:music? init-voice) init-voice) \getMusic #'() }
              >>
            #})
           (else
            #{
              <<
                { #meta }
                { \getMusicDeep {} #(glue-symbol (list name 'global) "-") $(if (ly:music? init-voice) init-voice) \getMusic #'() }
              >>
            #})
           )
       }
     #}))

\registerTemplate lalily.instrument.group
#(define-music-function (parser location piece options)(list? list?)
   (let ((groupmod (ly:assoc-get 'groupmod options #f #f))
         (staffs (ly:assoc-get 'staffs options '() #f)))
     #{
       \new StaffGroup \with {
         $(if (ly:context-mod? groupmod) groupmod)
       } $(make-music 'SimultaneousMusic 'elements
            (map (lambda (staff)
                   (let* ((key (assoc-get 'music (cdr staff) (list (car staff))))
                          (opts (assoc-set-all! (get-default-options (create-music-path #f key) location) (cdr staff)))
                          (instr (ly:assoc-get 'instrument opts #f #f))
                          (templ (cond
                                  ((symbol? instr) `(.. ,instr))
                                  ((list? instr) `(.. ,@instr))
                                  (else '(..))
                                  )))
                     ;(ly:message "~A -> ~A" piece key)
                     ;(ly:message "~A" (create-music-path #f key))
                     ;(ly:message "~A" (create-template-path #f templ))
                     ;(ly:message "~A" (format-alist opts))
                     #{ \callTemplate #templ #key #opts #}
                     )) staffs))
     #}))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% woodwind

\registerTemplate lalily.instrument.wood.oboe
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) parser location piece
     (assoc-set-all! options `((name . "oboe")
                               (midi-instrument . "oboe")
                               ))))

\registerTemplate lalily.instrument.wood.english-horn
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) parser location piece
     (assoc-set-all! options `((name . "english-horn")
                               (transposition . ,(ly:make-pitch -1 3 0)) ; F
                               (midi-instrument . "english horn")
                               ))))

\registerTemplate lalily.instrument.sax.sop
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) parser location piece
     (assoc-set-all! options `((name . "saxsop")
                               (transposition . ,(ly:make-pitch -1 6 -1/2)) ; B flat
                               (midi-instrument . "soprano sax")
                               ))))
\registerTemplate lalily.instrument.sax.alt
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) parser location piece
     (assoc-set-all! options `((name . "saxalt")
                               (transposition . ,(ly:make-pitch -1 2 -1/2)) ; E flat
                               (midi-instrument . "alto sax")
                               ))))

\registerTemplate lalily.instrument.sax.ten
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) parser location piece
     (assoc-set-all! options `((name . "saxten")
                               (transposition . ,(ly:make-pitch -2 6 -1/2)) ; B flat
                               (midi-instrument . "tenor sax")
                               ))))
\registerTemplate lalily.instrument.sax.bar
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) parser location piece
     (assoc-set-all! options `((name . "saxbar")
                               (transposition . ,(ly:make-pitch -2 2 -1/2)) ; E flat
                               (midi-instrument . "baritone sax")
                               ))))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% brass

\registerTemplate lalily.instrument.brass.trumpet
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) parser location piece
     (assoc-set-all! options `((default .
                                 ((name . "trumpet")
                                  (transposition . ,(ly:make-pitch -1 6 -1/2)) ; B flat
                                  (midi-instrument . "trumpet")
                                  (output-concert-pitch . #f)
                                  )))
       )))
\registerTemplate lalily.instrument.brass.trombone
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) parser location piece
     (assoc-set-all! options '((default .
                                 ((name . "trombone")
                                  (midi-instrument . "trombone")
                                  (clef . "bass")
                                  )))
       )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% strings

\registerTemplate lalily.instrument.strings.violin
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) parser location piece
     (assoc-set-all! options '((name . "violin")
                               (midi-instrument . "violin")
                               ))))

\registerTemplate lalily.instrument.strings.viola
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) parser location piece
     (assoc-set-all! options '((name . "viola")
                               (midi-instrument . "viola")
                               ))))

\registerTemplate lalily.instrument.strings.cello
#(define-music-function (parser location piece options)(list? list?)
   (call-template (create-template-path #f '(.. ..)) parser location piece
     (assoc-set-all! options '((name . "viola")
                               (midi-instrument . "viola")
                               ))))

