\version "2.18.0"

\registerTemplate lalily.vocal.init
#(define-music-function (parser location piece options)(list? list?)
   (let* ((localsym (assoc-get 'localsym options '(global) #f)) ; symbol for
           (deepsym (assoc-get 'deepsym options 'global-voice #f))
           (deepm #{ \getMusicDeep { \dynamicUp \autoBeamOff } #deepsym #}))
     #{
       \getMusic $deepm $localsym
     #}))


\registerTemplate lalily.vocal
#(define-music-function (parser location piece options)(list? list?)
   (let ((clef (assoc-get 'clef options "G" #f))
         (vocname (assoc-get 'vocname options #f #t))
         (staffname (assoc-get 'staffname options #f #f))
         (staff-mods (assoc-get 'staff-mods options #f #f))
         (voice-mods (assoc-get 'voice-mods options #f #f))
         (lyric-mods (assoc-get 'lyric-mods options #f #f))
         (repeats (assoc-get 'repeats options #f #f))
         (verses (assoc-get 'verses options #f #f)))
     (if (not (string? vocname))
         (let ((tmpname (glue-list piece "-")))
           (ly:input-warning location "using ~A as vocname!" tmpname)
           (set! vocname tmpname)
           ))
     (if (not (string? staffname)) (set! staffname (glue-list piece ":")))
     #{
       <<
         \new Staff = $staffname \with {
           $(if (ly:context-mod? staff-mods) staff-mods #{ \with {} #})
           \consists \editionEngraver $piece
         } \new Voice = $vocname \with {
           $(if (ly:context-mod? voice-mods) voice-mods #{ \with {} #})
         } <<
           \getMusicDeep {} #'meta
           { \callTemplate init #'() #'() \clef $clef \getMusic music }
         >>

         % TODO repeats
         $(if (list? verses)
              (make-music 'SimultaneousMusic
                'elements (map (lambda (v)
                                 #{
                                   \new Lyrics \with {
                                     $(if (ly:context-mod? lyric-mods) lyric-mods #{ \with {} #})
                                     $(let ((lyric-mods (assoc-get (glue-symbol `(lyric-mods ,v) "-") options #f #f)))
                                        (if (ly:context-mod? lyric-mods) lyric-mods #{ \with {} #}))
                                     \consists \editionEngraver $piece
                                   } \lyricsto $vocname { \getMusic #`(lyrics ,v) }
                                 #}) verses))
              #{
                \new Lyrics \with {
                  $(if (ly:context-mod? lyric-mods) lyric-mods #{ \with {} #})
                  \consists \editionEngraver $piece
                } \lyricsto $vocname { \getMusic lyrics }
              #})
       >>
     #}))

\clratree lalily_vocal_group_default
\addatree lalily_vocal_group_default sop.staff-mods \with { instrumentName = "Sopran" }
\addatree lalily_vocal_group_default alt.staff-mods \with { instrumentName = "Alt" }
\addatree lalily_vocal_group_default ten.staff-mods \with { instrumentName = "Tenor" }
\addatree lalily_vocal_group_default ten.clef "G_8"
\addatree lalily_vocal_group_default bas.staff-mods \with { instrumentName = "Bass" }
\addatree lalily_vocal_group_default bas.clef "bass"
\registerTemplate lalily.vocal.group
#(define-music-function (parser location piece options)(list? list?)
   (let ((staff-mods (ly:assoc-get 'staff-mods options #f #f))
         (group-mods (ly:assoc-get 'group-mods options #f #f))
         (staffs (ly:assoc-get 'staffs options lalily_vocal_group_default #f))
         (mensur (ly:assoc-get 'mensur options #f)))
     (define (get-staff-mods opt-staff-mods)
       (cond
        ((and (ly:context-mod? staff-mods)(ly:context-mod? opt-staff-mods))
         #{ \with { $opt-staff-mods $staff-mods } #})
        ((ly:context-mod? opt-staff-mods) opt-staff-mods)
        ((ly:context-mod? staff-mods) staff-mods)
        (else #f)
        ))
     #{
       \new StaffGroup \with {
         $(if (ly:context-mod? group-mods) group-mods)
         \consists \editionEngraver $piece
         \override BarLine.allow-span-bar = $(if mensur #t #f )
         \override BarLine.transparent = $(if mensur #t #f )
       } $(make-music 'SimultaneousMusic 'elements
            (map (lambda (staff)
                   (let* ((key (assoc-get 'music (cdr staff) (list (car staff))))
                          (vocname (string-append
                                    (assoc-get 'prefix (cdr staff) "")
                                    (assoc-get 'vocname (cdr staff) (glue-list key "-"))
                                    ))
                          (opts (assoc-set-all!
                                 (get-default-options (create-music-path #f key) location)
                                 (assoc-set-all! (cdr staff)
                                   `((vocname . ,vocname)
                                     (staff-mods . ,(get-staff-mods (assoc-get 'staff-mods (cdr staff)))))
                                   )
                                 ))
                          (instr (ly:assoc-get 'instrument opts #f #f))
                          (templ (cond
                                  ((symbol? instr) `(.. ,instr))
                                  ((list? instr) `(.. ,@instr))
                                  (else '(..))
                                  )))
                     #{ \callTemplate #templ #key #opts #}
                     )) staffs))
     #}))

