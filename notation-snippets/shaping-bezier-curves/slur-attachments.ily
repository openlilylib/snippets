\version "2.17.15"

\header {
  snippet-title = "adjusting slur attachments"
  snippet-author = "Janek Warchoł"
}

#(define (get-heads-ext note-column)
   ;; Return a combined extent of the noteheads in a notecolumn.
   ;; This is ugly, and should be implemented in C++.
   (let* ((elts (ly:grob-object note-column 'elements))
          (result empty-interval))
     (for-each
      (lambda (idx)
        (let ((elt (ly:grob-array-ref elts idx)))
          (if (grob::has-interface elt 'note-head-interface)
              (let*
               ((off (ly:grob-property elt 'Y-offset))
                (ext (ly:grob-property elt 'Y-extent)))
               (set! result (interval-union
                             result
                             (coord-translate ext off)))
               ))))
      (reverse (iota (ly:grob-array-length elts))))
     result))

#(define (get-stem note-column)
   ;; Return the stem from the notecolumn.
   ;; This is ugly, and should be implemented in C++.
   (let* ((elts (ly:grob-object note-column 'elements))
          (result #f))
     (for-each
      (lambda (idx)
        (let ((elt (ly:grob-array-ref elts idx)))
          (if (grob::has-interface elt 'stem-interface)
              (set! result elt))))
      (reverse (iota (ly:grob-array-length elts))))
     result))

#(define (empty-interval? interval)
   (or (equal? +inf.0 (car interval))
       (equal? -inf.0 (cdr interval))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#(define (calc-one-slur-end note-column slur-dir attach-to)
   (let* ((stem (get-stem note-column))
          (stem-dir (ly:grob-property stem 'direction))
          (stem-ext (ly:grob-property stem 'Y-extent))
          (column-ext (ly:grob-property note-column 'Y-extent))
          (heads-ext (get-heads-ext note-column)))

     (define inappropriate-stem?
       (or (empty-interval? stem-ext)
           (not (eq? stem-dir slur-dir))))

     ;; "raw" stem extent overlaps with the noteheads. We trim it
     ;; so that it represents only the sticking-out part.
     (define (normalize-stem-ext)
       (if (< (cdr heads-ext) (cdr stem-ext))
           (cons (cdr heads-ext)(cdr stem-ext))
           (cons (car stem-ext)(car heads-ext))))

     ;; when the column has an empty stem (e.g. a whole note),
     ;; or the stem is on the other side of the notehead than
     ;; the slur (e.g. in { c''2( f') } ), use an "artificial"
     ;; extent, 1-staffspace wide and adjacent to the noteheads.
     (define (make-artificial-ext)
       (if (eq? slur-dir UP)
           (cons (cdr column-ext)(+ 1 (cdr column-ext)))
           (cons (- (car column-ext) 1)(car column-ext))))

     (define (interpolate extent t)
       ;; dir=UP -> interpolate upwards
       ;; dir=DOWN -> interpolate downwards
       (begin (if (eq? slur-dir DOWN)(set! t (- 1 t)))
         (+ (* (- 1 t) (car extent))
           (* t (cdr extent)))))

     (define (attach-to-head)
       ;; 0.5 should be taken from a property
       (+ (* slur-dir 0.5)
         (if (eq? slur-dir UP)
             (cdr heads-ext)
             (car heads-ext))))

     (define (attach-to-stem)
       ;; 0.3 should be taken from a property
       (if (eq? slur-dir UP)
           (- (cdr stem-ext) 0.3)
           (+ (car stem-ext) 0.3)))

     (set! stem-ext (if inappropriate-stem?
                        (make-artificial-ext)
                        (normalize-stem-ext)))

     (cond ((number? attach-to)(interpolate stem-ext attach-to))
       ((eq? attach-to 'head)(attach-to-head))
       ((eq? attach-to 'stem)(if inappropriate-stem?
                                 (attach-to-head)
                                 (attach-to-stem)))
       (else (display "Error: unknown type")))))


attach =
#(define-music-function (parser location vals item)
   (scheme? symbol-list-or-music?)

   ;; convert 2-elem lists and single values into pairs,
   ;; convert strings to symbols.
   (define normalize-vals
     (let* ((paired (cond ((list? vals) (cons (first vals)(second vals)))
                      ((pair? vals) vals)
                      (else (cons vals vals)))))
       (cons (if (string? (car paired))
                 (string->symbol (car paired))
                 (car paired))
         (if (string? (cdr paired))
             (string->symbol (cdr paired))
             (cdr paired)))))

   (define (calc-positions grob)
     (let* ((orig (ly:grob-original grob))
            (l-bound (ly:spanner-bound grob LEFT))
            (r-bound (ly:spanner-bound grob RIGHT))
            (slur-dir (ly:grob-property grob 'direction))
            (specs normalize-vals))

       (cons (calc-one-slur-end l-bound slur-dir (car specs))
         (calc-one-slur-end r-bound slur-dir (cdr specs)))))
   #{ \tweak positions #calc-positions #item #})
