\version "2.17.29" % absolutely necessary!

\header {
  snippet-title = "Hairpins with added text"
  snippet-author = "Janek Warchoł and unknown author"
  snippet-source = "http://lsr.dsi.unimi.it/LSR/Item?id=233"
  snippet-description = \markup {
    This example provides a function to typeset a hairpin (de)crescendo
    with some additional text below it, such as "molto" or "poco".
    The added text will change the direction according to the direction
    of the hairpin. The Hairpin is aligned to DynamicText.
  }
  % add comma-separated tags to make searching more effective:
  tags = "hairpin, text"
  % is this snippet ready?  See meta/status-values.md
  status = ""
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

hairpinWithCenteredText =
#(define-music-function (parser location text horiz-align vert-align)
   (markup? number-or-string? number?)
   #{
     \once \override Hairpin.height = #(if (> 1 (abs vert-align)) 1.1 0.6666)
     \once \override Voice.Hairpin.after-line-breaking =
     #(lambda (grob)
        (let* ((stencil (ly:hairpin::print grob))
               (mrkup (grob-interpret-markup grob text))
               (par-y (ly:grob-parent grob Y))
               (grow-dir (ly:grob-property grob 'grow-direction))
               (horiz-side (if (and (string? horiz-align)
                                    (string=? "opening" horiz-align))
                               (if (eq? grow-dir 1) RIGHT LEFT)
                               horiz-align))
               (hairpin-dir (ly:grob-property par-y 'direction))
               (new-stencil (ly:stencil-aligned-to
                             ;; stencil-combine-at-edge doesn't work with fractional alignments,
                             ;; so we have to use stencil-add when we want to put text in hairpin.
                             (if (> 1 (abs vert-align))
                                 (ly:stencil-add
                                  (ly:stencil-aligned-to stencil X horiz-side)
                                  (ly:stencil-aligned-to
                                   (ly:stencil-aligned-to mrkup X horiz-side)
                                   Y (* -1 vert-align)))
                                 (ly:stencil-combine-at-edge
                                  (ly:stencil-aligned-to stencil X horiz-side)
                                  Y
                                  (* hairpin-dir vert-align)
                                  (ly:stencil-aligned-to mrkup X horiz-side)
                                  0.1))
                             X LEFT))
               (staff-space (ly:output-def-lookup (ly:grob-layout grob) 'staff-space))
               (staff-line-thickness
                (ly:output-def-lookup (ly:grob-layout grob) 'line-thickness))
               (grob-name (lambda (x) (assq-ref (ly:grob-property x 'meta) 'name)))
               (par-x (ly:grob-parent grob X))
               (dyn-text (eq? (grob-name par-x) 'DynamicText ))
               (dyn-text-stencil-x-length
                (if dyn-text
                    (interval-length
                     (ly:stencil-extent (ly:grob-property par-x 'stencil) X))
                    0))
               (x-shift
                (if dyn-text
                    (-
                     (+ staff-space dyn-text-stencil-x-length)
                     (* 0.5 staff-line-thickness)) 0)))

          (ly:grob-set-property! grob 'Y-offset 0)
          (ly:grob-set-property! grob 'stencil
            (ly:stencil-translate-axis
             new-stencil
             x-shift X))))
   #})
