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

% Custom type predicate:
#(define (number-or-list? x)
   (or (number? x) (list? x)))

scaleVerticalSpacingPageLayout =
#(define-scheme-function (parser location arg) (number-or-list?)
   "Multiplies the default values of the flexible vertical spacing paper 
    variables by the amounts specified in @{arg}, a number or alist. 
    Returns a paper block."
   (let*
    ;; Get scaling factors from arg.
    ((all (or (assq-ref arg 'all) (if (number? arg) arg)))
     (sys-sys (or (assq-ref arg 'system-system) all 1))
     (score-sys (or (assq-ref arg 'score-system) all 1))
     (mark-sys (or (assq-ref arg 'markup-system) all 1))
     (score-mark (or (assq-ref arg 'score-markup) all 1))
     (mark-mark (or (assq-ref arg 'markup-markup) all 1))
     (top-sys (or (assq-ref arg 'top-system) all 1))
     (top-mark (or (assq-ref arg 'top-markup) all 1))
     (last-bot (or (assq-ref arg 'last-bottom) all 1)))
    ;; Change settings by multiplying default values by the scaling factors.
    ;; If the default value is 1, don't bother multiplying by 1.
    #{
      \paper {
        system-system-spacing =
        #`((basic-distance . ,(* 12 sys-sys))
           (minimum-distance . ,(* 8 sys-sys))
           (padding . ,sys-sys))
        score-system-spacing =
        #`((basic-distance . ,(* 14 score-sys))
           (minimum-distance . ,(* 8 score-sys))
           (padding . ,score-sys))
        markup-system-spacing =
        #`((basic-distance . ,(* 5 mark-sys))
           (padding . ,(* 0.5 mark-sys)))
        score-markup-spacing =
        #`((basic-distance . ,(* 12 score-mark))
           (padding . ,(* 0.5 score-mark)))
        markup-markup-spacing =
        #`((basic-distance . ,mark-mark)
           (padding . ,(* 0.5 mark-mark)))
        top-system-spacing =
        % top-system-spacing: minimum-distance is 0
        #`((basic-distance . ,top-sys)
           (padding . ,top-sys))
        top-markup-spacing.padding = #top-mark
        % top-markup-spacing: basic-distance and minimum-distance are 0
        last-bottom-spacing =
        % last-bottom-spacing: minimum-distance is 0
        #`((basic-distance . ,last-bot)
           (padding . ,last-bot))
      }
    #}))


scaleVerticalSpacingInSystems =
#(define-scheme-function (parser location arg) (number-or-list?)
   "Multiplies the default values of the grob-properties that affect 
    flexible vertical spacing within systems by the amounts specified 
    in @{arg}, a number or alist.  Returns a layout block."
   ;; FretBoards do not set any grob-properties in VerticalAxisGroup,
   ;; so there's nothing to scale for them, so they are not included.
   (let*
    ;; Get scaling factors from arg.
    ((all (or (assq-ref arg 'all) (if (number? arg) arg)))
     (grp (or (assq-ref arg 'staff-grouper) all))
     (stf (or (assq-ref arg 'staff) (assq-ref arg 'staff-default-staff-staff) all))
     (crds (or (assq-ref arg 'chord-names) all))
     (dyn (or (assq-ref arg 'dynamics) (assq-ref arg 'dynamics-nonstaff-relatedstaff) all))
     (figb (or (assq-ref arg 'figured-bass) all))
     (lyr (or (assq-ref arg 'lyrics) all))
     (nn (or (assq-ref arg 'note-names) all))

     (grp-staff-staff (or (assq-ref arg 'StaffGrouper.staff-staff) grp))
     (grp-grp-staff (or (assq-ref arg 'StaffGrouper.staffgroup-staff) grp))
     (crds-non-rel (or (assq-ref arg 'chord-names-nonstaff-relatedstaff) crds))
     (crds-non-non (or (assq-ref arg 'chord-names-nonstaff-nonstaff) crds))
     (figb-non-rel (or (assq-ref arg 'figured-bass-nonstaff-relatedstaff) figb))
     (figb-non-non (or (assq-ref arg 'figured-bass-nonstaff-nonstaff) figb))
     (lyr-non-rel (or (assq-ref arg 'lyrics-nonstaff-relatedstaff) lyr))
     (lyr-non-non (or (assq-ref arg 'lyrics-nonstaff-nonstaff) lyr))
     (lyr-non-un (or (assq-ref arg 'lyrics-nonstaff-unrelatedstaff) lyr))
     (nn-non-rel (or (assq-ref arg 'note-names-nonstaff-relatedstaff) nn))
     (nn-non-non (or (assq-ref arg 'note-names-nonstaff-nonstaff) nn))
     (nn-non-un (or (assq-ref arg 'note-names-nonstaff-unrelatedstaff) nn)))
    ;; Create overrides by multiplying default values by the scaling factors.
    ;; If the default value is 1, don't bother multiplying by 1.
    #{
      \layout {
        \context {
          \Score {
            #(if grp-staff-staff #{
              \override StaffGrouper.staff-staff-spacing =
              #`((basic-distance . ,(* 9 grp-staff-staff))
                 (minimum-distance . ,(* 7 grp-staff-staff))
                 (padding . ,grp-staff-staff))
                 #})
            #(if grp-grp-staff #{
              \override StaffGrouper.staffgroup-staff-spacing =
              #`((basic-distance . ,(* 10.5 grp-grp-staff))
                 (minimum-distance . ,(* 8 grp-grp-staff))
                 (padding . ,grp-grp-staff))
                 #})
          }
        }
        \context {
          \Staff {
            #(if stf #{
              \override VerticalAxisGroup.default-staff-staff-spacing =
              #`((basic-distance . ,(* 9 stf))
                 (minimum-distance . ,(* 8 stf))
                 (padding . ,stf))
                 #})
          }
        }
        \context {
          \ChordNames {
            #(if crds-non-rel #{
              \override VerticalAxisGroup.nonstaff-relatedstaff-spacing.padding =
              #(* 0.5 crds-non-rel)
                 #})
            #(if crds-non-non #{
              \override VerticalAxisGroup.nonstaff-nonstaff-spacing.padding =
              #(* 0.5 crds-non-non)
                 #})
          }
        }
        \context {
          \Dynamics {
            #(if dyn #{
              \override VerticalAxisGroup.nonstaff-relatedstaff-spacing =
              #`((basic-distance . ,(* 5 dyn))
                 (padding . ,(* 0.5 dyn)))
                 #})
          }
        }
        \context {
          \FiguredBass {
            #(if figb #{
              \override VerticalAxisGroup.nonstaff-relatedstaff-spacing.padding =
              #(* 0.5 figb-non-rel)
                 #})
            #(if figb #{
              \override VerticalAxisGroup.nonstaff-nonstaff-spacing.padding =
              #(* 0.5 figb-non-non)
                 #})
          }
        }
        \context {
          \Lyrics {
            #(if lyr-non-rel #{
              \override VerticalAxisGroup.nonstaff-relatedstaff-spacing =
              #`((basic-distance . ,(* 5.5 lyr-non-rel))
                 (padding . ,(* 0.5 lyr-non-rel)))
                 #})
            #(if lyr-non-non #{
              \override VerticalAxisGroup.nonstaff-nonstaff-spacing =
              % basic-distance is 0
              #`((minimum-distance . ,(* 2.8 lyr-non-non))
                 (padding . ,(* 0.2 lyr-non-non)))
                 #})
            #(if lyr-non-un #{
              \override VerticalAxisGroup.nonstaff-unrelatedstaff-spacing.padding =
              #(* 1.5 lyr-non-un)
                 #})
          }
        }
        \context {
          \NoteNames {
            #(if nn-non-rel #{
              \override VerticalAxisGroup.nonstaff-relatedstaff-spacing =
              #`((basic-distance . ,(* 5.5 nn-non-rel))
                 (padding . ,(* 0.5 nn-non-rel)))
                 #})
            #(if nn-non-non #{
              \override VerticalAxisGroup.nonstaff-nonstaff-spacing =
              % basic-distance is 0
              #`((minimum-distance . ,(* 2.8 nn-non-non))
                 (padding . ,(* 0.2 nn-non-non)))
                 #})
            #(if nn-non-un #{
              \override VerticalAxisGroup.nonstaff-unrelatedstaff-spacing.padding =
              #(* 1.5 nn-non-un)
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
   (system-system . 1)
   (score-system . 1)
   (markup-system . 1)
   (score-markup . 1)
   (markup-markup . 1)
   (top-system . 1)
   (top-markup . 1)
   (last-bottom . 1))

% B1. scale all properties by the same amount
\scaleVerticalSpacingInSystems #1.5

% B2. scale properties for specific contexts
% (or of the StaffGrouper grob, which is not a context)
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
% (or of the StaffGrouper grob, which is not a context)
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

% B2 & B3
\scaleVerticalSpacingInSystems
#'((all . 1)
   (staff-grouper . 1)
   (staff . 1)
   (chord-names . 1)
   (dynamics . 1)
   (figured-bass . 1)
   (lyrics . 1)
   (note-names . 1)
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
