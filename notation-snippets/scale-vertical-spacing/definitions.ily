\version "2.19.13"

\header {
  snippet-title = "Scale vertical spacing"
  snippet-author = "Paul Morris"
  snippet-source = ""
  snippet-description = \markup {
    Provides two functions for scaling vertical spacing.  One scales the
    spacing of elements within systems (staff, staff groups, lyrics, chord
    names, etc.) and the other scales the spacing of page layout
    elements outside of systems (systems, top-level markups, etc.).
  }
  tags = "spacing, vertical-spacing, page-layout"
  status = "ready"
}

% custom type predicate:
#(define (number-or-association-list-of-symbols-and-numbers? x)
   (or (number? x)
       (and
        (list? x)
        (every symbol? (map car x))
        (every number? (map cdr x)))))


#(define (validate-input input valid-input-list caller)
   "Give a warning if an invalid property name is given as input."
   (if (list? input)
       (for-each
        (lambda (x)
          (if (not (memq (car x) valid-input-list))
              (ly:warning (_ "invalid property name supplied to ~S: ~S") caller (car x))))
        input)))


#(define (make-scaled-props defaults props-list input prop-lookup)
   "Takes an alist of default settings and multiplies the values by the
    scale factors in input, an alist of scale factors, and returns a new alist.
    prop-lookup is an alist that allows for two tiers of props for ...InSystems,
    and is simply #f for ...PageLayout."
   (let* ((all (assq-ref input 'all))
          ;; make a complete set of scaling factors for all props,
          ;; scaling factors are set by a cascade from the highest
          ;; priority source to the least using (or ... )
          (scale-factor-alist
           (map
            (lambda (prop)
              (cons prop
                (or
                 (assq-ref input prop)
                 (and (list? prop-lookup) (assq-ref input (assq-ref prop-lookup prop)))
                 all
                 #f)))
            props-list))
          ;; remove any items not set by user (prop value is #f)
          (scale-factor-alist2
           (filter
            (lambda (p) (not (equal? #f (cdr p))))
            scale-factor-alist)))
     ;; multiply defaults by scaling factors and return the new props alist
     (map
      (lambda (prop)
        (let* ((prop-name (car prop))
               (mult (cdr prop))
               (dfs (assq-ref defaults prop-name)))
          (cons prop-name
            (map
             (lambda (df)
               (if (eq? (car df) 'stretchability)
                   df
                   (cons (car df) (* mult (cdr df)))))
             dfs))))
      scale-factor-alist2)))


scaleVerticalSpacingPageLayout =
#(define-void-function (parser location input)
   (number-or-association-list-of-symbols-and-numbers?)
   "Multiplies the default values of the flexible vertical spacing paper
    variables by the amounts specified in @{input}, a number or alist.
    Returns a paper block. See:
    http://lilypond.org/doc/v2.18/Documentation/notation/flexible-vertical-spacing-paper-variables"
   (let*
    ((input2
      (if (number? input)
          (list (cons 'all input))
          input))
     (paper (ly:parser-lookup parser '$defaultpaper))
     (props-list
      '(system-system-spacing
        score-system-spacing
        markup-system-spacing
        score-markup-spacing
        markup-markup-spacing
        top-system-spacing
        top-markup-spacing
        last-bottom-spacing))

     (defaults
      (map
       (lambda (x) (cons x (ly:output-def-lookup paper x)))
       props-list))

     (valid-input-list (concatenate (list '(all) props-list)))

     ;; generate new props by multiplying defaults by scaling factors
     (nprops
      (make-scaled-props defaults props-list input2 #f)))
    ;; let* body

    ;; give warning on bad input
    (validate-input input valid-input-list "scaleVerticalSpacingPageLayout")
    ;; set paper output-def variables
    (for-each
     (lambda (x)
       (display (ly:output-def-lookup paper (car x) "yoo")) (newline)
       (ly:output-def-set-variable! paper (car x) (cdr x))
       (display (ly:output-def-lookup paper (car x) "yoo")) (newline))
     nprops)))


scaleVerticalSpacingInSystems =
#(define-scheme-function (parser location input)
   (number-or-association-list-of-symbols-and-numbers?)
   "Multiplies the default values of the grob-properties that affect
    flexible vertical spacing within systems by the amount(s) specified
    in @{input}, a number or alist. Returns a layout block. See:
    http://lilypond.org/doc/v2.18/Documentation/notation/flexible-vertical-spacing-within-systems"
   ;; FretBoards do not set any grob-properties in VerticalAxisGroup,
   ;; so there's nothing to scale for them, so they are not included.
   (let*
    ((input2
      (if (number? input)
          (list (cons 'all input))
          input))
     ;; (layout (ly:parser-lookup parser '$defaultlayout))
     ;; default values of 0 are omitted since they can't be scaled
     (defaults
      '((staff-grouper-staff-staff . ((basic-distance . 9) (minimum-distance . 7) (padding . 1) (stretchability . 5)))
        (staff-grouper-staffgroup-staff . ((basic-distance . 10.5) (minimum-distance . 8) (padding . 1) (stretchability . 9)))
        (staff-default-staff-staff . ((basic-distance . 9) (minimum-distance . 8) (padding . 1)))
        (chord-names-nonstaff-relatedstaff . ((padding . 0.5)))
        (chord-names-nonstaff-nonstaff . ((padding . 0.5)))
        (dynamics-nonstaff-relatedstaff . ((basic-distance . 5) (padding . 0.5)))
        (figured-bass-nonstaff-relatedstaff . ((padding . 0.5)))
        (figured-bass-nonstaff-nonstaff . ((padding . 0.5)))
        (lyrics-nonstaff-relatedstaff . ((basic-distance . 5.5) (padding . 0.5) (stretchability . 1)))
        (lyrics-nonstaff-nonstaff . ((minimum-distance . 2.8) (padding . 0.2) (stretchability . 0)))
        (lyrics-nonstaff-unrelatedstaff . ((padding . 0.5)))
        (note-names-nonstaff-relatedstaff . ((basic-distance . 5.5) (padding . 0.5) (stretchability . 1)))
        (note-names-nonstaff-nonstaff . ((minimum-distance . 2.8) (padding . 0.2) (stretchability . 0)))
        (note-names-nonstaff-unrelatedstaff . ((padding . 1.5)))))
     (prop-lookup
      '((staff-grouper-staff-staff . staff-grouper)
        (staff-grouper-staffgroup-staff . staff-grouper)
        (staff-default-staff-staff . staff)
        (chord-names-nonstaff-relatedstaff . chord-names)
        (chord-names-nonstaff-nonstaff . chord-names)
        (dynamics-nonstaff-relatedstaff . dynamics)
        (figured-bass-nonstaff-relatedstaff . figured-bass)
        (figured-bass-nonstaff-nonstaff . figured-bass)
        (lyrics-nonstaff-relatedstaff . lyrics)
        (lyrics-nonstaff-nonstaff . lyrics)
        (lyrics-nonstaff-unrelatedstaff . lyrics)
        (note-names-nonstaff-relatedstaff . note-names)
        (note-names-nonstaff-nonstaff . note-names)
        (note-names-nonstaff-unrelatedstaff . note-names)))
     (props-list (map car defaults))
     (valid-input-list (concatenate (list '(all) props-list
                                      (delete-duplicates (map cdr prop-lookup)))))
     ;; generate new props by multiplying defaults by scaling factors
     (nprops
          (make-scaled-props defaults props-list input2 prop-lookup)))
    ;; let* body

    ;; give warning on bad input
    (validate-input input valid-input-list "scaleVerticalSpacingInSystems")
    #{
      \layout {
        \context {
          \Score {
            #(if (assq-ref nprops 'staff-grouper-staff-staff) #{
              \override StaffGrouper.staff-staff-spacing =
              #(assq-ref nprops 'staff-grouper-staff-staff)
                 #})
            #(if (assq-ref nprops 'staff-grouper-staffgroup-staff) #{
              \override StaffGrouper.staffgroup-staff-spacing =
              #(assq-ref nprops 'staff-grouper-staffgroup-staff)
                 #})
          }
        }
        \context {
          \Staff {
            #(if (assq-ref nprops 'staff-default-staff-staff) #{
              \override VerticalAxisGroup.default-staff-staff-spacing =
              #(assq-ref nprops 'staff-default-staff-staff)
                 #})
          }
        }
        \context {
          \ChordNames {
            #(if (assq-ref nprops 'chord-names-nonstaff-relatedstaff) #{
              \override VerticalAxisGroup.nonstaff-relatedstaff-spacing =
              #(assq-ref nprops 'chord-names-nonstaff-relatedstaff)
                 #})
            #(if (assq-ref nprops 'chord-names-nonstaff-nonstaff) #{
              \override VerticalAxisGroup.nonstaff-nonstaff-spacing =
              #(assq-ref nprops 'chord-names-nonstaff-nonstaff)
                 #})
          }
        }
        \context {
          \Dynamics {
            #(if (assq-ref nprops 'dynamics-nonstaff-relatedstaff) #{
              \override VerticalAxisGroup.nonstaff-relatedstaff-spacing =
              #(assq-ref nprops 'dynamics-nonstaff-relatedstaff)
                 #})
          }
        }
        \context {
          \FiguredBass {
            #(if (assq-ref nprops 'figured-bass-nonstaff-relatedstaff) #{
              \override VerticalAxisGroup.nonstaff-relatedstaff-spacing =
              #(assq-ref nprops 'figured-bass-nonstaff-relatedstaff)
                 #})
            #(if (assq-ref nprops 'figured-bass-nonstaff-nonstaff) #{
              \override VerticalAxisGroup.nonstaff-nonstaff-spacing =
              #(assq-ref nprops 'figured-bass-nonstaff-nonstaff)
                 #})
          }
        }
        \context {
          \Lyrics {
            #(if (assq-ref nprops 'lyrics-nonstaff-relatedstaff) #{
              \override VerticalAxisGroup.nonstaff-relatedstaff-spacing =
              #(assq-ref nprops 'lyrics-nonstaff-relatedstaff)
                 #})
            #(if (assq-ref nprops 'lyrics-nonstaff-nonstaff) #{
              \override VerticalAxisGroup.nonstaff-nonstaff-spacing =
              #(assq-ref nprops 'lyrics-nonstaff-nonstaff)
                 #})
            #(if (assq-ref nprops 'lyrics-nonstaff-unrelatedstaff) #{
              \override VerticalAxisGroup.nonstaff-unrelatedstaff-spacing =
              #(assq-ref nprops 'lyrics-nonstaff-unrelatedstaff)
                 #})
          }
        }
        \context {
          \NoteNames {
            #(if (assq-ref nprops 'note-names-nonstaff-relatedstaff) #{
              \override VerticalAxisGroup.nonstaff-relatedstaff-spacing =
              #(assq-ref nprops 'note-names-nonstaff-relatedstaff)
                 #})
            #(if (assq-ref nprops 'note-names-nonstaff-nonstaff) #{
              \override VerticalAxisGroup.nonstaff-nonstaff-spacing =
              #(assq-ref nprops 'note-names-nonstaff-nonstaff)
                 #})
            #(if (assq-ref nprops 'note-names-nonstaff-unrelatedstaff) #{
              \override VerticalAxisGroup.nonstaff-unrelatedstaff-spacing =
              #(assq-ref nprops 'note-names-nonstaff-unrelatedstaff)
                 #})
          }
        }
      }
    #}))


%%%%%%%%%%%%%%%%%%%%%%%%%
% EXAMPLE USAGE
% GOOD FOR CUTTING AND PASTING
%%%%%%%%%%%%%%%%%%%%%%%%%

%{

   % A1. scale all page layout variables by the same amount

   \scaleVerticalSpacingPageLayout #1.5

   % A2. scale specific page layout variables

   \scaleVerticalSpacingPageLayout
   #'((all . 1)
      (system-system-spacing . 1)
      (score-system-spacing . 1)
      (markup-system-spacing . 1)
      (score-markup-spacing . 1)
      (markup-markup-spacing . 1)
      (top-system-spacing . 1)
      (top-markup-spacing . 1)
      (last-bottom-spacing . 1))

   % B1. scale all "in systems" properties by the same amount

   \scaleVerticalSpacingInSystems #1.5

   % B2. scale properties for specific contexts
   % (or of the StaffGrouper grob -- not a context)

   \scaleVerticalSpacingInSystems
   #'((all . 1)
      (staff-grouper . 1)
      (staff . 1)
      (chord-names . 1)
      (dynamics . 1)
      (figured-bass . 1)
      (lyrics . 1)
      (note-names . 1))

   % B3. scale specific properties within specific contexts
   % (or of the StaffGrouper grob -- not a context)

   \scaleVerticalSpacingInSystems
   #'((all . 1)
      (staff-grouper-staff-staff . 1)
      (staff-grouper-staffgroup-staff . 1)
      (staff-default-staff-staff . 1) ;; same as (staff . 1)
      (chord-names-nonstaff-relatedstaff . 1)
      (chord-names-nonstaff-nonstaff . 1)
      (dynamics-nonstaff-relatedstaff . 1) ;; same as (dynamics . 1)
      (figured-bass-nonstaff-relatedstaff . 1)
      (figured-bass-nonstaff-nonstaff . 1)
      (lyrics-nonstaff-relatedstaff . 1)
      (lyrics-nonstaff-nonstaff . 1)
      (lyrics-nonstaff-unrelatedstaff . 1)
      (note-names-nonstaff-relatedstaff . 1)
      (note-names-nonstaff-nonstaff . 1)
      (note-names-nonstaff-unrelatedstaff . 1))
%}