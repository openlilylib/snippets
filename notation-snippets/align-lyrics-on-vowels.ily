\version "2.16.2" % absolutely necessary!

\header {
  snippet-title = "Aligning lyrics on vowels"
  snippet-author = ""
  snippet-description = \markup {
    Align lyrics so that the vowel will line up with the note.
  }
  status = "broken, unfinished, undocumented"
  % TODO:
  % doesn't work with 2.17.25 - fix. 
  % Find author.
  % Document.
  % add vowels from other languages (e.g. polish)
  % check if it works with different notehead widths, and if the
  % vowel is really centered (regardless of glyph width)

  % add comma-separated tags to make searching more effective:
  tags = "lyrics, alignment, vowel, vowel alignment"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define vowel-set (list->char-set (string->list "AEIOUYÅÄÖaeiouyåäö")))

#(define (width grob text-string)
   (let* (
          (layout (ly:grob-layout grob))
          (props (ly:grob-alist-chain grob (ly:output-def-lookup layout 'text-font-defaults))))
     (cdr (ly:stencil-extent (ly:text-interface::interpret-markup layout props (markup text-string)) X))))

#(define (center-on-vowel grob)
   (let* ((syllable (ly:grob-property-data grob 'text))
          (vowel-count (string-count syllable vowel-set))
          (vowel-position (string-index syllable vowel-set))
          (prevowel (substring syllable 0 vowel-position))
          (vowel (substring syllable vowel-position (+ vowel-position 1)))
          (prevowel-width (width grob prevowel))
          (vowel-width (width grob vowel))
          (note-width (interval-length (ly:grob-property
                                        (ly:grob-parent grob X)
                                        'X-extent))))
     (- (/ (- note-width vowel-width) 2) prevowel-width)))
