\version "2.16.2"

\header {
  snippet-title = "Displaying anchors (reference points) of objects"
  snippet-author = "Thomas Morley"
  % taken from this thread on the mailing list:
  % http://lists.gnu.org/archive/html/lilypond-user/2013-03/msg01048.html
  snippet-description = \markup {
    In LilyPond, all graphical objects have an anchor (a reference point).
    What is a reference point?  It's a special point that defines the
    object's position.  Think about geometry: if you have to define where a
    figure is placed on a plane, you'll usually say something like
    “the lower left corner of this square has coordinates (0, 2)” or “the
    center of this circle is at (-1, 3)”. “Lower left corner” and “center”
    would be the reference points for square and circle.

    This snippet shows where anchors of particular objects are located.
  }
  % add comma-separated tags to make searching more effective:
  tags = "preview mode, draft mode, anchor, reference point, refpoint, alignment, offset"
  % is this snippet ready?  See meta/status-values.md
  status = "ready"
}

% Define appearance
#(cond ((not (defined? 'debug-grob-anchor-dotcolor))
        (define debug-grob-anchor-dotcolor red)))
% Which grobs to print the dot to?
% Possible values:
% - 'all-grobs
% - Name of a grob (as symbol)
% - List of grob names
#(cond ((not (defined? 'debug-grob-anchor-groblist))
        ;(define debug-grob-anchor-groblist '(NoteHead Stem))))
        ;(define debug-grob-anchor-groblist 'NoteHead )))
        (define debug-grob-anchor-groblist 'all-grobs)))


#(define (add-dot)
   (lambda (grob)
     (let* ((layout (ly:grob-layout grob))
            (props (layout-extract-page-properties layout))
            (font
             (ly:paper-get-font layout
               (cons '((font-encoding . fetaMusic)) props)))
            ;; Get the stencil-procedure from ly:grob-basic-properties.
            ;; If any, use it to create the stencil.
            (function (assoc-get 'stencil (ly:grob-basic-properties grob)))
            (stencil (if function (function grob) point-stencil))
            ;; Get the grob-name and create a text-stencil.
            ;; Read out the y-length for later translate.
            (grob-name-proc
             (lambda (x) (assq-ref (ly:grob-property x 'meta) 'name)))
            (grob-name (grob-name-proc grob))
            (grob-string (if (symbol? grob-name)
                             (symbol->string grob-name)
                             "no name"))
            (ref-text-stil (grob-interpret-markup grob
                             (markup
                              #:with-color red
                              #:normal-text
                              #:abs-fontsize 6
                              (string-append "   " grob-string))))
            (ref-text-stil-length
             (interval-length (ly:stencil-extent ref-text-stil Y)))
            (dot (stencil-with-color 
                  (ly:font-get-glyph font "dots.dot")
                  debug-grob-anchor-dotcolor))
            (dot-length
             (interval-length (ly:stencil-extent dot X)))
            (dot-stil
             (ly:stencil-translate-axis dot (/ dot-length -2) X)))

       ;; If there's a grob with stencil-procedure and a valid stencil is
       ;; created, add the red-dot-stil and an optional text-stencil.
       (if (and function (ly:stencil? stencil) (grob::is-live? grob))
           (ly:grob-set-property! grob 'stencil
             (ly:stencil-add
              stencil
              dot-stil))))))

% needs to be here for 2.16.2
#(define-public (symbol-list-or-symbol? x)
   (if (list? x)
       (every symbol? x)
       (symbol? x)))

#(define (add-dot-to-grobs l)
   ;; possible values for l:
   ;;   'all-grobs (adds red-dots to all grobs, where possible)
   ;;          this will naturally cause collisions,
   ;;   a single grob-name, must be a symbol,
   ;;   a list of grob-names,
   ;;   anything else (returns the unchanged original stencil)
   ;;  TODO: How to apply it once?
   (let ((grobs-to-consider
          (cond ((eq? l 'all-grobs)
                 all-grob-descriptions)
            ((symbol? l)
             (list (assoc l all-grob-descriptions)))
            ((list? l)
             (map
              (lambda (grob)
                (assoc grob all-grob-descriptions))
              l))
            (else '()))))
     (lambda (context)
       (let loop ((x grobs-to-consider))
         (if (not (null? x))
             (let ((grob-name (caar x)))
               (ly:context-pushpop-property
                context
                grob-name
                'after-line-breaking
                (add-dot))
               (loop (cdr x))))))))

printAnchors =
#(define-music-function (parser location s-or-l)(symbol-list-or-symbol?)
   "
       Will add a dot to the stencil's ref-point of the
 specified grob(s).
 Valid input for s-or-l:
      @code{'all-grobs}, (adds red-dots to all grobs, where possible), this will
          naturally cause collisions
      a single grob-name, must be a symbol,
      a list of grob-names.
 To avoid bleeding-overs any context has to be initiated explicitly.
"
#{
  \applyContext #(add-dot-to-grobs s-or-l)
#})

%% For single use:

#(define addDot
   (lambda (grob)
     (let* ((function (assoc-get 'stencil (ly:grob-basic-properties grob)))
            (stencil (if function (function grob) point-stencil))
            (layout (ly:grob-layout grob))
            (props (layout-extract-page-properties layout))
            (font
             (ly:paper-get-font layout
               (cons '((font-encoding . fetaMusic)) props)))
            (dot (ly:font-get-glyph font "dots.dot"))
            (dot (stencil-with-color 
                  (ly:font-get-glyph font "dots.dot")
                  debug-grob-anchor-dotcolor))
            (dot-length
             (interval-length (ly:stencil-extent dot X)))
            (dot-stil
             (ly:stencil-translate-axis dot (/ dot-length -2) X)))

       (if (and function (ly:stencil? stencil) (grob::is-live? grob))
           (ly:grob-set-property! grob 'stencil
             (ly:stencil-add
              stencil
              dot-stil))))))

%% Overriding grobs must be defined separately.
%% Don't forget to specify the context if necessary.
onceDotScript = \once \override Script #'after-line-breaking = #addDot
