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
#(define (number-or-list-of-symbols-and-numbers? x)
   (or (number? x)
       (and
        (list? x)
        (every symbol? (map car x))
        (every number? (map last x)))))

#(define (validate-input input valid-input-list caller)
   "Give a warning if an invalid property name is given as input."
   (if (list? input)
       (for-each
        (lambda (x)
          (if (not (memq (car x) valid-input-list))
              (ly:warning (_ "invalid argument supplied to ~S: ~S") caller (car x))))
        input)))

#(define (extract-context-name full-name)
   "Given StaffGrouper-staff-staff-spacing, returns StaffGrouper, etc."
   (string->symbol (car (string-split (symbol->string full-name) #\-))))

#(define (make-scaled-props defaults props-list input compound-prop)
   "Takes an alist of default settings and multiplies the values by the
    scale factors in input, an alist of scale factors, and returns a new alist.
    compound-prop is a boolean, #t for two tiers of props for ...InSystems,
    and #f for single tier for ...PageLayout."
   (let* ((all (assq-ref input 'all))
          ;; make a complete set of scaling factors for all props,
          ;; scaling factors are set by a cascade from the highest
          ;; priority source to the lowest using (or ... )
          (scale-factor-alist
           (map
            (lambda (prop)
              (cons prop
                (or
                 (assq-ref input prop)
                 (and compound-prop (assq-ref input (extract-context-name prop)))
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
               (mult (second prop))
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
   (number-or-list-of-symbols-and-numbers?)
   "Multiplies the default values of the flexible vertical spacing paper
    variables by the amounts specified in @{input}, a number or list.
    Returns a paper block. See:
    http://lilypond.org/doc/v2.18/Documentation/notation/flexible-vertical-spacing-paper-variables"
   (let*
    ((input2
      (if (number? input)
          (list (list 'all input))
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
    ;; begin let* body

    ;; give warning on bad input
    (validate-input input valid-input-list "scaleVerticalSpacingPageLayout")

    ;; set paper output-def variables
    (for-each
     (lambda (x)
       (ly:output-def-set-variable! paper (car x) (cdr x)))
     nprops)))


#(define (assemble-prop-names arg)
   "Takes a list of lists and merges the first two symbols in each 
    list, if the list begins with two symbols. This normalizes 
    input for compound in-system properties."
   (map
    (lambda (x)
      (if (symbol? (second x))
          (append
           (list (string->symbol
                  (string-append
                   (symbol->string (car x))
                   "-"
                   (symbol->string (second x)))))
           (list-tail x 2))
          x))
    arg))


scaleVerticalSpacingInSystems =
#(define-scheme-function (parser location input)
   (number-or-list-of-symbols-and-numbers?)
   "Multiplies the default values of the grob-properties that affect
    flexible vertical spacing within systems by the amount(s) specified
    in @{input}, a number or list. Returns a layout block. See:
    http://lilypond.org/doc/v2.18/Documentation/notation/flexible-vertical-spacing-within-systems"
   ;; FretBoards do not set any grob-properties in VerticalAxisGroup,
   ;; so there's nothing to scale for them, so they are not included.
   (let*
    ((input2
      (if (number? input)
          (list (list 'all input))
          (assemble-prop-names input)))
     (staff-props '((basic-distance . 9) (minimum-distance . 8) (padding . 1)))
     ;; default values of 0 are omitted since they don't scale
     (defaults
      (list
       '(StaffGrouper-staff-staff-spacing (basic-distance . 9) (minimum-distance . 7) (padding . 1) (stretchability . 5))
       '(StaffGrouper-staffgroup-staff-spacing (basic-distance . 10.5) (minimum-distance . 8) (padding . 1) (stretchability . 9))
       '(ChordNames-nonstaff-relatedstaff-spacing (padding . 0.5))
       '(ChordNames-nonstaff-nonstaff-spacing (padding . 0.5))
       '(Dynamics-nonstaff-relatedstaff-spacing (basic-distance . 5) (padding . 0.5))
       '(FiguredBass-nonstaff-relatedstaff-spacing (padding . 0.5))
       '(FiguredBass-nonstaff-nonstaff-spacing (padding . 0.5))
       '(Lyrics-nonstaff-relatedstaff-spacing (basic-distance . 5.5) (padding . 0.5) (stretchability . 1))
       '(Lyrics-nonstaff-nonstaff-spacing (minimum-distance . 2.8) (padding . 0.2) (stretchability . 0))
       '(Lyrics-nonstaff-unrelatedstaff-spacing (padding . 0.5))
       '(NoteNames-nonstaff-relatedstaff-spacing (basic-distance . 5.5) (padding . 0.5) (stretchability . 1))
       '(NoteNames-nonstaff-nonstaff-spacing (minimum-distance . 2.8) (padding . 0.2) (stretchability . 0))
       '(NoteNames-nonstaff-unrelatedstaff-spacing (padding . 1.5))
       (append '(Staff-default-staff-staff-spacing) staff-props)
       (append '(DrumStaff-default-staff-staff-spacing) staff-props)
       (append '(GregorianTranscriptionStaff-default-staff-staff-spacing) staff-props)
       (append '(KievanStaff-default-staff-staff-spacing) staff-props)
       (append '(MensuralStaff-default-staff-staff-spacing) staff-props)
       (append '(PetrucciStaff-default-staff-staff-spacing) staff-props)
       (append '(RhythmicStaff-default-staff-staff-spacing) staff-props)
       (append '(TabStaff-default-staff-staff-spacing) staff-props)
       (append '(VaticanaStaff-default-staff-staff-spacing) staff-props)))

     (ctx-props-list (map car defaults))
     (ctx-list (delete-duplicates (map extract-context-name ctx-props-list)))
     (valid-input-list (append '(all) ctx-props-list ctx-list))
     (nprops '()))
    ;; begin let* body

    ;; give warning on bad input (before generating new props)
    (validate-input input2 valid-input-list "scaleVerticalSpacingInSystems")

    ;; generate new props by multiplying defaults by scaling factors
    (set! nprops (make-scaled-props defaults ctx-props-list input2 #t))

    #{
      \layout {
        \context {
          \Score {
            #(if (assq-ref nprops 'StaffGrouper-staff-staff-spacing) #{
              \override StaffGrouper.staff-staff-spacing =
              #(assq-ref nprops 'StaffGrouper-staff-staff-spacing)
                 #})
            #(if (assq-ref nprops 'StaffGrouper-staff-staff-spacing) #{
              \override StaffGrouper.staffgroup-staff-spacing =
              #(assq-ref nprops 'StaffGrouper-staffgroup-staff-spacing)
                 #})
          }
        }
        \context {
          \ChordNames {
            #(if (assq-ref nprops 'ChordNames-nonstaff-relatedstaff-spacing) #{
              \override VerticalAxisGroup.nonstaff-relatedstaff-spacing =
              #(assq-ref nprops 'ChordNames-nonstaff-relatedstaff-spacing)
                 #})
            #(if (assq-ref nprops 'ChordNames-nonstaff-nonstaff-spacing) #{
              \override VerticalAxisGroup.nonstaff-nonstaff-spacing =
              #(assq-ref nprops 'ChordNames-nonstaff-nonstaff-spacing)
                 #})
          }
        }
        \context {
          \Dynamics {
            #(if (assq-ref nprops 'Dynamics-nonstaff-relatedstaff-spacing) #{
              \override VerticalAxisGroup.nonstaff-relatedstaff-spacing =
              #(assq-ref nprops 'Dynamics-nonstaff-relatedstaff-spacing)
                 #})
          }
        }
        \context {
          \FiguredBass {
            #(if (assq-ref nprops 'FiguredBass-nonstaff-relatedstaff-spacing) #{
              \override VerticalAxisGroup.nonstaff-relatedstaff-spacing =
              #(assq-ref nprops 'FiguredBass-nonstaff-relatedstaff-spacing)
                 #})
            #(if (assq-ref nprops 'FiguredBass-nonstaff-nonstaff-spacing) #{
              \override VerticalAxisGroup.nonstaff-nonstaff-spacing =
              #(assq-ref nprops 'FiguredBass-nonstaff-nonstaff-spacing)
                 #})
          }
        }
        \context {
          \Lyrics {
            #(if (assq-ref nprops 'Lyrics-nonstaff-relatedstaff-spacing) #{
              \override VerticalAxisGroup.nonstaff-relatedstaff-spacing =
              #(assq-ref nprops 'Lyrics-nonstaff-relatedstaff-spacing)
                 #})
            #(if (assq-ref nprops 'Lyrics-nonstaff-nonstaff-spacing) #{
              \override VerticalAxisGroup.nonstaff-nonstaff-spacing =
              #(assq-ref nprops 'Lyrics-nonstaff-nonstaff-spacing)
                 #})
            #(if (assq-ref nprops 'Lyrics-nonstaff-unrelatedstaff-spacing) #{
              \override VerticalAxisGroup.nonstaff-unrelatedstaff-spacing =
              #(assq-ref nprops 'Lyrics-nonstaff-unrelatedstaff-spacing)
                 #})
          }
        }
        \context {
          \NoteNames {
            #(if (assq-ref nprops 'NoteNames-nonstaff-relatedstaff-spacing) #{
              \override VerticalAxisGroup.nonstaff-relatedstaff-spacing =
              #(assq-ref nprops 'NoteNames-nonstaff-relatedstaff-spacing)
                 #})
            #(if (assq-ref nprops 'NoteNames-nonstaff-nonstaff-spacing) #{
              \override VerticalAxisGroup.nonstaff-nonstaff-spacing =
              #(assq-ref nprops 'NoteNames-nonstaff-nonstaff-spacing)
                 #})
            #(if (assq-ref nprops 'NoteNames-nonstaff-unrelatedstaff-spacing) #{
              \override VerticalAxisGroup.nonstaff-unrelatedstaff-spacing =
              #(assq-ref nprops 'NoteNames-nonstaff-unrelatedstaff-spacing)
                 #})
          }
        }
        \context {
          \Staff {
            #(if (assq-ref nprops 'Staff-default-staff-staff-spacing) #{
              \override VerticalAxisGroup.default-staff-staff-spacing =
              #(assq-ref nprops 'Staff-default-staff-staff-spacing)
                 #})
          }
        }
        \context {
          \DrumStaff {
            #(if (assq-ref nprops 'DrumStaff-default-staff-staff-spacing) #{
              \override VerticalAxisGroup.default-staff-staff-spacing =
              #(assq-ref nprops 'DrumStaff-default-staff-staff-spacing)
                 #})
          }
        }
        \context {
          \GregorianTranscriptionStaff {
            #(if (assq-ref nprops 'GregorianTranscriptionStaff-default-staff-staff-spacing) #{
              \override VerticalAxisGroup.default-staff-staff-spacing =
              #(assq-ref nprops 'GregorianTranscriptionStaff-default-staff-staff-spacing)
                 #})
          }
        }
        \context {
          \KievanStaff {
            #(if (assq-ref nprops 'KievanStaff-default-staff-staff-spacing) #{
              \override VerticalAxisGroup.default-staff-staff-spacing =
              #(assq-ref nprops 'KievanStaff-default-staff-staff-spacing)
                 #})
          }
        }
        \context {
          \MensuralStaff {
            #(if (assq-ref nprops 'MensuralStaff-default-staff-staff-spacing) #{
              \override VerticalAxisGroup.default-staff-staff-spacing =
              #(assq-ref nprops 'MensuralStaff-default-staff-staff-spacing)
                 #})
          }
        }
        \context {
          \PetrucciStaff {
            #(if (assq-ref nprops 'PetrucciStaff-default-staff-staff-spacing) #{
              \override VerticalAxisGroup.default-staff-staff-spacing =
              #(assq-ref nprops 'PetrucciStaff-default-staff-staff-spacing)
                 #})
          }
        }
        \context {
          \RhythmicStaff {
            #(if (assq-ref nprops 'RhythmicStaff-default-staff-staff-spacing) #{
              \override VerticalAxisGroup.default-staff-staff-spacing =
              #(assq-ref nprops 'RhythmicStaff-default-staff-staff-spacing)
                 #})
          }
        }
        \context {
          \TabStaff {
            #(if (assq-ref nprops 'TabStaff-default-staff-staff-spacing) #{
              \override VerticalAxisGroup.default-staff-staff-spacing =
              #(assq-ref nprops 'TabStaff-default-staff-staff-spacing)
                 #})
          }
        }
        \context {
          \VaticanaStaff {
            #(if (assq-ref nprops 'VaticanaStaff-default-staff-staff-spacing) #{
              \override VerticalAxisGroup.default-staff-staff-spacing =
              #(assq-ref nprops 'VaticanaStaff-default-staff-staff-spacing)
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
   #'((all 1)
      (system-system-spacing 1)
      (score-system-spacing 1)
      (markup-system-spacing 1)
      (score-markup-spacing 1)
      (markup-markup-spacing 1)
      (top-system-spacing 1)
      (top-markup-spacing 1)
      (last-bottom-spacing 1))

   % B1. scale all "in systems" properties by the same amount

   \scaleVerticalSpacingInSystems #1.5

   % B2. scale properties for specific contexts
   % (or of the StaffGrouper grob -- not a context)

   \scaleVerticalSpacingInSystems
   #'((all 1)
      (StaffGrouper 1)
      (ChordNames 1)
      (Dynamics 1)
      (FiguredBass 1)
      (Lyrics 1)
      (NoteNames 1)
      (Staff 1)
      (DrumStaff 1)
      (GregorianTranscriptionStaff 1)
      (KievanStaff 1)
      (MensuralStaff 1)
      (PetrucciStaff 1)
      (RhythmicStaff 1)
      (TabStaff 1)
      (VaticanaStaff 1))

   % B3. scale specific properties within specific contexts
   % (or of the StaffGrouper grob -- not a context)
   %
   % Items that have only a single propery to scale
   % (i.e. Dynamics, Staff, DrumStaff, and all other staff types...)
   % are not included below, because:
   % (Staff default-staff-staff-spacing 1) is the same as (Staff 1),
   % (Dynamics nonstaff-relatedstaff-spacing 1) is the same as (Dynamics 1),
   % etc. either version will work, but the shorter version is simpler.

   \scaleVerticalSpacingInSystems
   #'((all 1)
      (StaffGrouper staff-staff-spacing 1)
      (StaffGrouper staffgroup-staff-spacing 1)
      (ChordNames nonstaff-relatedstaff-spacing 1)
      (ChordNames nonstaff-nonstaff-spacing 1)
      (FiguredBass nonstaff-relatedstaff-spacing 1)
      (FiguredBass nonstaff-nonstaff-spacing 1)
      (Lyrics nonstaff-relatedstaff-spacing 1)
      (Lyrics nonstaff-nonstaff-spacing 1)
      (Lyrics nonstaff-unrelatedstaff-spacing 1)
      (NoteNames nonstaff-relatedstaff-spacing 1)
      (NoteNames nonstaff-nonstaff-spacing 1)
      (NoteNames nonstaff-unrelatedstaff-spacing 1))
%}
