\version "2.18.0" % absolutely necessary!

\header {
  snippet-title = "Magnetic snapping lyric syllables"
  snippet-author = "David Nalesnik and Mike Solomon,"
  snippet-source = "http://lists.gnu.org/archive/html/lilypond-user/2014-03/msg00489.html"
  snippet-description = \markup {
    This snippet handles lyric syllables that belong to one word together
    and ensures that there are no irritating gaps between them (solves issue 2458).
  }
  % add comma-separated tags to make searching more effective:
  tags = "lyrics, syllable, gap, hyphen"
  % is this snippet ready?  See meta/status-values.md
  status = "undecided"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% ADD NEW GROB INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(ly:add-interface
  'lyric-word-interface
  "A word of lyrics. Includes syllables and hyphens."
  '(text-items))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% CREATE NEW GROB PROPERTY %%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
     (text-items ,list? "Syllables and hyphens of a word of lyrics")))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%% ADD DEFINITION OF GROB %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (add-grob-definition grob-name grob-entry)
   (let* ((meta-entry   (assoc-get 'meta grob-entry))
          (class        (assoc-get 'class meta-entry))
          (ifaces-entry (assoc-get 'interfaces meta-entry)))
     (set-object-property! grob-name 'translation-type? list?)
     (set-object-property! grob-name 'is-grob? #t)
     (set! ifaces-entry (append (case class
                                  ((Item) '(item-interface))
                                  ((Spanner) '(spanner-interface))
                                  ((Paper_column) '((item-interface
                                                     paper-column-interface)))
                                  ((System) '((system-interface
                                               spanner-interface)))
                                  (else '(unknown-interface)))
                          ifaces-entry))
     (set! ifaces-entry (uniq-list (sort ifaces-entry symbol<?)))
     (set! ifaces-entry (cons 'grob-interface ifaces-entry))
     (set! meta-entry (assoc-set! meta-entry 'name grob-name))
     (set! meta-entry (assoc-set! meta-entry 'interfaces
                        ifaces-entry))
     (set! grob-entry (assoc-set! grob-entry 'meta meta-entry))
     (set! all-grob-descriptions
           (cons (cons grob-name grob-entry)
             all-grob-descriptions))))

#(add-grob-definition
  'LyricWord
  `(;(stencil . ,ly:lyric-word::print)
     (meta . ((class . Spanner)
              (interfaces . (lyric-hyphen-interface
                             lyric-word-interface
                             text-interface))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (add-bound-item spanner item)
   (if (null? (ly:spanner-bound spanner LEFT))
       (ly:spanner-set-bound! spanner LEFT item)
       (ly:spanner-set-bound! spanner RIGHT item)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ENGRAVER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collectlyricwordEngraver =
% Collect lyric syllables and hyphens into words. (LyricExtender?)
% The bounds of a LyricWord should be LyricText grobs, when available.
% When a LyricWord consists of a single syllable, the left and right bounds
% should be the same grob.
% When a spanner is broken, the ends not attached to LyricText grobs should
% attach to NonMusicalPaperColumn, as with any spanner.
#(lambda (context)
   (let ((word-bits '()) ; holds syllables and hyphens
          (word '()) ; LyricWord grob we're building
          (collect #f)) ; signal to end word and begin another
     (make-engraver

      (acknowledgers
       ((lyric-syllable-interface engraver grob source-engraver)
        (set! collect #t)
        (set! word-bits (append word-bits (list grob)))
        (if (ly:grob? word)
            (add-bound-item word grob)))
       ((lyric-hyphen-interface engraver grob source-engraver)
        (let* ((props (ly:grob-basic-properties grob))
               (meta (assoc-get 'meta props))
               (name (assoc-get 'name meta)))
          ; don't collect LyricSpace
          ; use it as our signal to end o word/start a new one
          (if (eq? name 'LyricSpace)
              (set! collect #f)
              (set! word-bits (append word-bits (list grob)))))))

      ((process-music trans)
       (if (and collect (pair? word-bits))
           (begin
            (if (not (ly:grob? word))
                (set! word (ly:engraver-make-grob trans 'LyricWord '())))
            ; car should always be a LyricText grob, but maybe a check is in order
            (add-bound-item word (car word-bits))
            (for-each
             (lambda (x)
               (ly:pointer-group-interface::add-grob word 'text-items x))
             word-bits)))

       (if (not collect)
           (begin
            (if (ly:grob? word)
                (begin
                 (if (pair? word-bits)
                     (begin
                      (for-each
                       (lambda (x)
                         (ly:pointer-group-interface::add-grob word 'text-items x))
                       word-bits)
                      (if (null? (ly:spanner-bound word RIGHT))
                          (ly:spanner-set-bound!
                           word RIGHT
                           (car word-bits)))))
                 (set! word (ly:engraver-make-grob trans 'LyricWord '()))
                 (set! collect #t)))

            (if (not (ly:grob? word))
                (begin
                 (set! word (ly:engraver-make-grob trans 'LyricWord '()))
                 (if (pair? word-bits)
                     (begin
                      (ly:spanner-set-bound! word LEFT (car word-bits))
                      (for-each
                       (lambda (x)
                         (ly:pointer-group-interface::add-grob word 'text-items x))
                       word-bits)
                      (if (null? (ly:spanner-bound word RIGHT))
                          (ly:spanner-set-bound! word RIGHT (car word-bits)))))
                 (set! word '())))))

       (set! word-bits '())))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (compress-pair syl-a hyphen syl-b threshold)
   (let* ((hyphen-sten (ly:lyric-hyphen::print hyphen))
          (hyphen-ex
           (if (ly:stencil? hyphen-sten)
               (ly:stencil-extent hyphen-sten X)
               (cons (/ threshold -2) (/ threshold 2)))))
     (if (> (interval-length hyphen-ex) threshold)
         '() ; no compression--DO NOTHING!

         (let*
          ((syl-a-text (ly:grob-property syl-a 'text))
           (syl-a-text (if (markup? syl-a-text) syl-a-text (markup syl-a-text)))
           (syl-b-text (ly:grob-property syl-b 'text))
           (syl-b-text (if (markup? syl-b-text) syl-b-text (markup syl-b-text)))
           ; add zero width non-joiner between syllables
           (full-text (make-concat-markup (list syl-a-text "‌" syl-b-text))))

          (set! (ly:grob-property syl-a 'text) full-text)
          (set! (ly:grob-property syl-b 'text) empty-markup)
          (set! (ly:grob-property syl-a 'stencil) lyric-text::print)
          (set! (ly:grob-property syl-b 'stencil) lyric-text::print)
          (set! (ly:grob-property hyphen 'stencil) empty-stencil)))))

#(define (lyric-word-compressor threshold)
   (lambda (grob) ; LyricWord
     (let* ((items (ly:grob-object grob 'text-items))
            (item-list (ly:grob-array->list items)))
       (if (> (length item-list) 1) ; do nothing to monosyllabic words
           (let* ((text-grobs
                   (filter
                    (lambda (item)
                      (grob::has-interface item 'lyric-syllable-interface))
                    item-list))
                  (hyphen-grobs
                   (filter
                    (lambda (item)
                      (grob::has-interface item 'lyric-hyphen-interface))
                    item-list)))

             (define (helper seed tx-list hy-list)
               (if (and (pair? (cdr tx-list))
                        (pair? hy-list))
                   (let ((next-syl (cadr tx-list))
                         (hyphen (car hy-list)))
                     (compress-pair seed hyphen next-syl threshold)
                     (if (equal? empty-markup (ly:grob-property next-syl 'text))
                         (helper seed (cdr tx-list) (cdr hy-list))
                         (helper (cadr tx-list) (cdr tx-list) (cdr hy-list))))))

             (helper (car text-grobs) text-grobs hyphen-grobs))))))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXAMPLE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%% SOME OTHER FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (dim-hack grob ax)
   (let* ((elts (ly:grob-object grob 'text-items))
          (common (ly:grob-common-refpoint-of-array grob elts ax))
          (rel (ly:relative-group-extent elts common ax))
          (off (ly:grob-relative-coordinate grob common ax)))
     (coord-translate rel (- off))))

#(define (height-hack grob)
   (dim-hack grob Y))

#(define (width-hack grob)
   (dim-hack grob X))

#(define (ly:lyric-word::underline grob)
   (let* ((height (height-hack grob))
          (width (width-hack grob)))

     (make-line-stencil 0.1 (car width) 0 (cdr width) 0)))

#(define (ly:lyric-word::boxer grob)
   (let* ((yext (height-hack grob))
          (xext (width-hack grob))
          (thick 0.1))

     (ly:stencil-add
      (make-filled-box-stencil xext (cons (- (car yext) thick) (car yext)))
      (make-filled-box-stencil xext (cons (cdr yext) (+ (cdr yext) thick)))
      (make-filled-box-stencil (cons (cdr xext) (+ (cdr xext) thick)) yext)
      (make-filled-box-stencil (cons (- (car xext) thick) (car xext)) yext))))

wordunderline = \once \override LyricWord.stencil = #ly:lyric-word::underline
wordbox = \once \override LyricWord.stencil = #ly:lyric-word::boxer

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
