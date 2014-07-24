\version "2.18.2"

#(define-public (add-notation-font fontnode name music-str brace-str factor)
  (begin 
    (add-music-fonts fontnode 
      name music-str brace-str 
      feta-design-size-mapping factor)
    fontnode))

\paper {
  #(define notation-fonts
    (list
      (list 'amadeus "amadeus" "emmentaler")
      (list 'cadence "cadence" "emmentaler")
      (list 'emmentaler "emmentaler" "emmentaler")
      (list 'gonville "gonville" "gonville")
      (list 'gutenberg "gutenberg1939" "gonville")
      (list 'haydn "haydn" "gonville")
      (list 'lilyjazz "lilyjazz" "emmentaler")
      (list 'paganini "paganini" "emmentaler")
      (list 'profondo "profondo" "emmentaler")
    ))
    
  #(begin 
    (for-each
      (lambda (tup)
        (add-notation-font fonts 
          (car tup) ; font identifier
          (cadr tup) ; notation font
          (caddr tup) ; brace font
          (/ staff-height pt 20)))
      notation-fonts))
}
