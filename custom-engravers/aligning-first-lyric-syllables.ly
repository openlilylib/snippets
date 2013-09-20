\version "2.16.2"

\header {
  snippet-title = "Aligning first lyric syllables in each system together"
  snippet-author = "David Nalesnik"
  % see http://lists.gnu.org/archive/html/lilypond-user/2013-05/msg00706.html
  snippet-description = \markup {
  }
  % add comma-separated tags to make searching more effective:
  tags = "lyrics, alignment, hymn, stanzas, column"
  % is this snippet ready?  See meta/status-values.md
  status = "undocumented"
}

%%%%%%%%%%%%%%%%%%%%%%

#(define (define-grob-property symbol type? description)
   (if (not (equal? (object-property symbol 'backend-doc) #f))
       (ly:error (_ "symbol ~S redefined") symbol))

   (set-object-property! symbol 'backend-type? type?)
   (set-object-property! symbol 'backend-doc description)
   symbol)

#(map
  (lambda (x)
    (apply define-grob-property x))

  `(
    (tagged ,boolean? "is this grob marked?")
    (syllables ,array? "the lyric syllables at a timestep")
    ))

%%%%%%%%%%%%%%%%%%%%%%%%

#(define (Lyric_text_align_engraver ctx)
   "If the property 'tagged is set, collect all lyric syllables at that
      timestep in the grob-array `syllables'"
(let ((syl '()))
  (make-engraver
   (acknowledgers
    ((lyric-syllable-interface trans grob source)
     (set! syl (cons grob syl))))
   ((stop-translation-timestep trans)
    (if (any (lambda (x) (eq? #t (ly:grob-property x 'tagged)))
          syl)
        (for-each
         (lambda (x)
           (for-each
            (lambda (y)
              (ly:pointer-group-interface::add-grob x 'syllables y))
            syl))
         syl))
    (set! syl '())))))

#(define (X-offset-callback grob)
   (let* ((target (ly:grob-object grob 'syllables))
          (target
           (if (ly:grob-array? target)
               (ly:grob-array->list target)
               '())))
     (if (pair? target)
         (let ((longest
                (car
                 (sort target
                   (lambda (x y)
                     (> (interval-length
                         (ly:stencil-extent
                          (grob-interpret-markup
                           grob (ly:grob-property x 'text))
                          X))
                        (interval-length
                         (ly:stencil-extent
                          (grob-interpret-markup
                           grob (ly:grob-property y 'text))
                          X))))))))
           (if (eq? grob longest)
               ; if our grob has the longest syllable, return its default
               ; value for 'X-offset
               (ly:self-alignment-interface::aligned-on-x-parent grob)
               (ly:grob-property longest 'X-offset)))
         (ly:self-alignment-interface::aligned-on-x-parent grob))))

tagIt = \once \override Lyrics.LyricText #'tagged = ##t

\layout {
  ragged-right = ##f
  \context {
    \Lyrics
    \override LyricText #'X-offset = #X-offset-callback
  }
  \context {
    \Score
    \consists #Lyric_text_align_engraver
  }
}

\score {
  \new Staff <<
    \new Voice = A {
      \relative c' {
        c d e2 \bar "|."
      }
    }
    \new Lyrics \lyricsto A {
      \tagIt Do -- mi -- nus,
    }

    \new Lyrics \lyricsto A {
      Cant -- a me
    }
    \new Lyrics \lyricsto A {
      Syll -- a -- bum!
    }
  >>
}