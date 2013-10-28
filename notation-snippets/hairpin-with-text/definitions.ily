\version "2.14.2" % absolutely necessary!

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
#(define-music-function (parser location text) (markup?)
#{
  \once \override Voice.Hairpin #'after-line-breaking = 
    #(lambda (grob)
      (let* ((stencil (ly:hairpin::print grob))
             (par-y (ly:grob-parent grob Y))
             (dir (ly:grob-property par-y 'direction))
             (new-stencil (ly:stencil-aligned-to
               (ly:stencil-combine-at-edge
                 (ly:stencil-aligned-to stencil X CENTER)
                 Y dir
                 (ly:stencil-aligned-to (grob-interpret-markup grob $text) X CENTER))
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
