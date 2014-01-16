\version "2.17.3"

% TODO: add oll/snippets headers, add example.

%{
  Scheme engraver that places double bar lines before time signature changes
  Provided by Jan-Peter Voigt
  Presumably written by David Nalesnik
%}

DbBars = #(lambda (context)
            (let ((time-signature '())
                  (last-fraction #f))

              `((process-music
                 . ,(lambda (trans)
                      (let ((frac (ly:context-property context 'timeSignatureFraction)))
                        (if (and (null? time-signature)
                                 (not (equal? last-fraction frac))
                                 (fraction? frac))
                            (begin
                             (ly:context-set-property! context 'whichBar "||")
                             (set! last-fraction frac))))))

                (stop-translation-timestep
                 . ,(lambda (trans)
                      (set! time-signature '()))))))