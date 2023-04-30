\version "2.18.0"

\header {
  snippet-title = "merge-rests-engraver"
  snippet-author = "Jay Anderson"
  snippet-source =
    "http://www.mail-archive.com/lilypond-user%40gnu.org/msg69608.html"
  snippet-description = \markup {
    Merge rests of equal duration in different voice
  }
  tags = "merge, rest, rests, voice, voices"
  status = "ready"
}

#(define has-one-or-less (lambda (lst) (or (null? lst) (null? (cdr lst)))))
#(define has-at-least-two (lambda (lst) (not (has-one-or-less lst))))
#(define (all-equal lst pred)
   (or (has-one-or-less lst)
       (and (pred (car lst) (cadr lst)) (all-equal (cdr lst) pred))))

#(define merge-rests-engraver
   (lambda (context)
     (let ((rest-same-length
            (lambda (rest-a rest-b)
              (eq? (ly:grob-property rest-a 'duration-log)
                   (ly:grob-property rest-b 'duration-log))))
           (rests '()))
       `((start-translation-timestep . ,(lambda (trans)
                                          (set! rests '())))
         (stop-translation-timestep . ,(lambda (trans)
                                         (if (and (has-at-least-two
                                                   rests) (all-equal rests rest-same-length))
                                             (begin
                                              (for-each
                                               (lambda (rest)
                                                 (ly:grob-set-property! rest 'Y-offset 0))
                                               rests)
                                              (for-each
                                               (lambda (rest)
                                                 (ly:grob-set-property! (ly:grob-object rest 'dot) 'dot-count 0))
                                               (cdr rests))))))
         (acknowledgers
          (rest-interface . ,(lambda (engraver grob source-engraver)
                               (if (eq? 'Rest (assoc-ref
                                               (ly:grob-property grob 'meta) 'name))
                                   (set! rests (cons grob rests))))))))))

#(define merge-mmrests-engraver
   (lambda (context)
     (let* ((mmrest-same-length
             (lambda (rest-a rest-b)
               (eq? (ly:grob-property rest-a 'measure-count)
                    (ly:grob-property rest-b 'measure-count))))
            (merge-mmrests
             (lambda (rests)
               (if (all-equal rests mmrest-same-length)
                   (let ((offset (if (eq? (ly:grob-property (car rests)
                                            'measure-count) 1) 1 0)))
                     (for-each
                      (lambda (rest) (ly:grob-set-property! rest
                                       'Y-offset offset))
                      rests)))))
            (curr-rests '())
            (rests '()))
       `((start-translation-timestep . ,(lambda (trans)
                                          (set! curr-rests '())))
         (stop-translation-timestep . ,(lambda (trans)
                                         (if (has-at-least-two curr-rests)
                                             (set! rests (cons curr-rests rests)))))
         (finalize . ,(lambda (translator)
                        (for-each merge-mmrests rests)))
         (acknowledgers
          (rest-interface . ,(lambda (engraver grob source-engraver)
                               (if (eq? 'MultiMeasureRest (assoc-ref
                                                           (ly:grob-property grob 'meta) 'name))
                                   (set! curr-rests (cons grob curr-rests))))))))))
