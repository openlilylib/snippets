\version "2.16.2"

\header {
  snippet-title = "Aligning first lyric syllables in each system together"
  snippet-author = "David Nalesnik"
  % see http://lists.gnu.org/archive/html/lilypond-user/2013-05/msg00706.html
  snippet-description = \markup {
    \wordwrap {
       This snippet allows you to left-align selected columns of syllables in vocal
       music with multiple verses.  The column will be positioned so that the longest
       syllable is centered on the note.  This is particularly useful at the beginnings
       of lines.  To request the alignment at a particular location, you need to
       "/"mark/"" one of the syllables there.  This snippet is also a demonstration of
       defining and using a tag of sorts, here to control a Scheme engraver.
    }
  }
  % add comma-separated tags to make searching more effective:
  tags = "lyrics, alignment, hymn, stanzas, column"
  % is this snippet ready?  See meta/status-values.md
  status = "undocumented"
}

%%%%%%%%%%%%%%%%%%%
%   THE SNIPPET   %
%%%%%%%%%%%%%%%%%%%

% Thw following function is taken from `scm/define-grob-properties.scm'.  We apply it
% to the new properties so LilyPond won't complain about the use of undocumented
% properties.

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


#(define (Lyric_text_align_engraver ctx)
  "If the property 'tagged is set, collect all lyric syllables at that
timestep in the grob-array 'syllables"
  (let (; We will collect LyricText items here.
        (syl '()))
    (make-engraver
      (acknowledgers
        ((lyric-syllable-interface trans grob source)
         (set! syl (cons grob syl))))
        ((stop-translation-timestep trans)
         ; This ensures that callbacks will have access to all of the LyricText grobs
         ; in a column though any of the LyricText grobs in that column.
         (if (any (lambda (x) (eq? #t (ly:grob-property x 'tagged)))
                  syl)
             (for-each (lambda (x) 
                         (for-each
                           (lambda (y)
                             (ly:pointer-group-interface::add-grob x 'syllables y))
                           syl))
                       syl))
         ; clear our cache of syllables in readiness for another group
         (set! syl '())))))

#(define (X-offset-callback grob)
  (let* (; This is the grob-array we created to hold targeted syllables.
         (target (ly:grob-object grob 'syllables))
         ; Check is necessary because engraver has only created 'syllables
         ; grob-array in response to tag 
         (target
           (if (ly:grob-array? target)
               (ly:grob-array->list target)
               '()))
         ; A procedure to determine the size of a text stencil:
         (length (lambda (item) 
                   (interval-length
                     (ly:stencil-extent
                        (grob-interpret-markup grob (ly:grob-property item 'text))
                        X)))))
    (if (pair? target) ; Do we have a 'syllables grob-array?
        (let ((longest
                (fold
                  (lambda (elem prev)
                    (if (> (length elem) (length prev)) elem prev))
                  (car target)
                  target)))
          (if (eq? grob longest)
              ; If our grob has the longest syllable, return its default
              ; value for 'X-offset.
              (ly:self-alignment-interface::aligned-on-x-parent grob)
              ; Otherwise, give it the X-offset value of the longest syllable.
              (ly:grob-property longest 'X-offset)))
        
        ; If no grob-array, return the default function for 'X-offset
        (ly:self-alignment-interface::aligned-on-x-parent grob))))
