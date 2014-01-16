%\version "2.17.3" ?

% TODO: add oll/snippets headers, add example.


%{
  Scheme engraver that corrects stem-side staccato alignment if there are other articulations present.
  Written by David Nalesnik
%}

StaccatoCorrector =
#(lambda (context)
   (let ((articulations '())
         (staccato '()))
     (make-engraver
      (acknowledgers ((script-interface engraver grob source-engraver)
                      (set! articulations (cons grob articulations))
                      (if (equal? (ly:prob-property (event-cause grob) 'articulation-type) "staccato")
                          (set! staccato grob))))
      ((stop-translation-timestep trans)
       (if (and (ly:grob? staccato)
                (or (> (length articulations) 1)
                    (boolean? (ly:prob-property (event-cause staccato) 'parenthesize))))
           (set! (ly:grob-property staccato 'toward-stem-shift) 0))
       (set! articulations '())
       (set! staccato '())))))