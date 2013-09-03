\version "2.17.25"

#(define (repeat-stencil grob)
   (let* ((orig (ly:grob-original grob))
          (siblings (ly:spanner-broken-into orig)) ; have we been split?
          (bounds (ly:grob-array->list (ly:grob-object grob 'columns)))
          (refp (ly:grob-system grob))
          ;; we use the first and/or last NonMusicalPaperColumn grob(s) of
          ;; a system in the event that a MeasureCounter spanner is broken
          (all-cols (ly:grob-array->list (ly:grob-object refp 'columns)))
          (all-cols
           (filter
            (lambda (col) (eq? #t (ly:grob-property col 'non-musical)))
            all-cols))
          (left-bound
           (if (or (null? siblings) ; spanner is unbroken
                   (eq? grob (car siblings))) ; or the first piece
               (car bounds)
               (car all-cols)))
          (right-bound
           (if (or (null? siblings)
                   (eq? grob (car (reverse siblings))))
               (car (reverse bounds))
               (car (reverse all-cols))))
          (elts-L (ly:grob-array->list (ly:grob-object left-bound 'elements)))
          (elts-R (ly:grob-array->list (ly:grob-object right-bound 'elements)))
          (break-alignment-L
           (filter
            (lambda (elt) (grob::has-interface elt 'break-alignment-interface))
            elts-L))
          (break-alignment-R
           (filter
            (lambda (elt) (grob::has-interface elt 'break-alignment-interface))
            elts-R))
          (break-alignment-L-ext (ly:grob-extent (car break-alignment-L) refp X))
          (break-alignment-R-ext (ly:grob-extent (car break-alignment-R) refp X))
          (num
           (make-concat-markup
            (list
             (number->string (ly:grob-property grob 'count-from))
             "x")))
          (num
           (if (or (null? siblings)
                   (eq? grob (car siblings)))
               num
               (make-parenthesize-markup num)))
          (num (grob-interpret-markup grob num))
          (num-stil-ext-X (ly:stencil-extent num X))
          (num-stil-ext-Y (ly:stencil-extent num Y))
          (num (ly:stencil-aligned-to num X (ly:grob-property grob 'self-alignment-X)))
          (num
           (ly:stencil-translate-axis
            num
            (+ (interval-length break-alignment-L-ext)
              (* 0.5
                (- (car break-alignment-R-ext)
                  (cdr break-alignment-L-ext))))
            X))
          (bracket-L
           (markup
            #:path
            0.1 ; line-thickness
            `((moveto 0.5 ,(* 0.5 (interval-length num-stil-ext-Y)))
              (lineto ,(* 0.5
                         (- (car break-alignment-R-ext)
                           (cdr break-alignment-L-ext)
                           (interval-length num-stil-ext-X)))
                ,(* 0.5 (interval-length num-stil-ext-Y)))
              (closepath)
              (rlineto 0.0 -1.0))))
          (bracket-R
           (markup
            #:path
            0.1
            `((moveto ,(* 0.5
                         (- (car break-alignment-R-ext)
                           (cdr break-alignment-L-ext)
                           (interval-length num-stil-ext-X)))
                ,(* 0.5 (interval-length num-stil-ext-Y)))
              (lineto 0.5
                ,(* 0.5 (interval-length num-stil-ext-Y)))
              (closepath)
              (rlineto 0.0 -1.0))))
          (bracket-L (grob-interpret-markup grob bracket-L))
          (bracket-R (grob-interpret-markup grob bracket-R))
          (num (ly:stencil-combine-at-edge num X LEFT bracket-L 0.4))
          (num (ly:stencil-combine-at-edge num X RIGHT bracket-R 0.4)))
     num))


\relative c'' {
  \time 8/4
  \override Staff.MeasureCounter.font-encoding = #'latin1
  \override Staff.MeasureCounter.font-size = 0
  \override Staff.MeasureCounter.stencil = #repeat-stencil
  \once \override Staff.MeasureCounter.count-from = 3
  \override Staff.DynamicLineSpanner.padding = 2
  \startMeasureCount
  d!2~\p d16 \noBeam es,\mf bes4 b''!4.\f c,!2.\mp
  \stopMeasureCount
  d!2\p
}

\layout {
  \context {
    \Staff
    \consists #Measure_counter_engraver
    \remove "Time_signature_engraver"
  }
}
