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


#(define (get-attachment-interval note-column slur-dir)
   (let* ((column-ext (ly:grob-property note-column 'Y-extent))
          (stem (get-stem note-column))
          (stem-dir (ly:grob-property stem 'direction))
          (stem-ext (ly:grob-property stem 'Y-extent)))

     ;; "raw" stem extent overlaps with the noteheads. We trim it
     ;; so that it represents only the sticking-out part.
     (define (normalize-stem-ext s-extent column)
       (let* ((heads-ext (get-heads-ext column)))
         (if (< (cdr heads-ext) (cdr s-extent))
             (cons (cdr heads-ext)(cdr s-extent))
             (cons (car s-extent)(car heads-ext)))))

     ;; when the column has an empty stem (e.g. a whole note),
     ;; or the stem is on the other side of the notehead than
     ;; the slur (e.g. in { c''2( f') } ), use an "artificial"
     ;; extent, 1-staffspace wide and adjacent to the noteheads.
     (define (make-artificial-ext base-ext dir)
       (if (eq? dir UP)
           (cons (cdr base-ext)(+ 1 (cdr base-ext)))
           (cons (- (car base-ext) 1)(car base-ext))))

     (if (or (empty-interval? stem-ext)
             (not (eq? stem-dir slur-dir)))
         (make-artificial-ext column-ext slur-dir)
         (normalize-stem-ext stem-ext note-column))))

#(define (interpolate extent param dir)
   ;; dir=UP -> interpolate upwards
   ;; dir=DOWN -> interpolate downwards
   (let* ((t (if (eq? dir UP)
                 param
                 (- 1 param)))
          (ptA (car extent))
          (ptB (cdr extent)))
     (+ (* (- 1 t) ptA)
       (* t ptB))))

attach =
#(define-music-function (parser location attaches item)
   (pair? symbol-list-or-music?)
   (define (calc-pos grob)
     (let* ((orig (ly:grob-original grob))
            (l-bound (ly:spanner-bound grob LEFT))
            (r-bound (ly:spanner-bound grob RIGHT))
            (slur-dir (ly:grob-property grob 'direction))

            (l-attach (get-attachment-interval l-bound slur-dir))
            (r-attach (get-attachment-interval r-bound slur-dir))

            (l-pos (interpolate l-attach (car attaches) slur-dir))
            (r-pos (interpolate r-attach (cdr attaches) slur-dir)))

       (format #t "\nl-attach ~a" l-attach)
       (format #t ", l-pos ~a" l-pos)
       (format #t "  r-attach ~a" r-attach)
       (format #t ", r-pos ~a" r-pos)
       (cons l-pos r-pos)))
   #{ \tweak positions #calc-pos #item #})
