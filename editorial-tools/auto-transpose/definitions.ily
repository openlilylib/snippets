\version "2.18.2"

\header {
  snippet-title = "auto-transpose"
  snippet-author = "Jan-Peter Voigt"
  snippet-description = \markup {
    \wordwrap {
      This engraver transposes music accordingly to 'instrumentTransposition'
      and 'music-concert-pitch' plus 'print-concert-pitch'.
      The example is working from concert-pitch to instrument-pitch, but MIDI is not right in the other direction. (TODO)
    }
  }
  tags = "transpose,transposition"
  status = "unknown"
}

% taken from "scm/define-context-properties.scm"
#(define (translator-property-description symbol type? description)
   (if (not (and
             (symbol? symbol)
             (procedure? type?)
             (string? description)))
       (throw 'init-format-error))

   (if (not (equal? #f (object-property symbol 'translation-doc)))
       (ly:error (_ "symbol ~S redefined" symbol)))

   (set-object-property! symbol 'translation-type? type?)
   (set-object-property! symbol 'translation-doc description)
   (set! all-translation-properties (cons symbol all-translation-properties))
   symbol)
% add context properties descriptions
%   music-concert-pitch
%   print-concert-pitch
#(translator-property-description 'music-concert-pitch boolean? "music is in concert pitch")
#(translator-property-description 'print-concert-pitch boolean? "print it in concert pitch")

% engraver to automatically transpose music
autoTransposeEngraver =
#(lambda (context)
   (let ((base (ly:make-pitch 0 0 0)) ; pitch c'
          (lasttransp (ly:context-property context 'instrumentTransposition))) ; last instrument transposition
     (define (cond-transp engraver music)
       (let ((mcp (ly:context-property context 'music-concert-pitch)) ; music is in concert-pitch t/f
              (pcp (ly:context-property context 'print-concert-pitch)) ; print it in concert-pitch t/f
              (transp (ly:context-property context 'instrumentTransposition)) ; instrument transposition
              (keysig (ly:context-property context 'keySignature)) ; key-signature
              (tonic (ly:context-property context 'tonic))) ; key-signature tonic

         (define (do-transp m)
           (cond
            ; music in concert-pitch / display in instrument pitch
            ((and mcp (not pcp) (ly:pitch? transp))
             (ly:music-transpose m (ly:pitch-diff base transp)))
            ; music in instrument pitch / display in concert pitch
            ((and (not mcp) pcp (ly:pitch? transp))
             (ly:music-transpose m transp))
            ))
         
         ; TODO: if instrument transposition changed, produce key signature
         (if (not (equal? transp lasttransp))
             (let ((key-sig (make-music 'KeyChangeEvent 'pitch-alist keysig 'tonic tonic)))
               (ly:broadcast (ly:context-event-source context)
                 (ly:make-stream-event 'key-change-event `((music-cause . ,key-sig)) ))
               ))
         (set! lasttransp transp)
         
         ; execute transposition
         (do-transp music)
         ))
     
     ; create engraver
     (make-engraver
      (listeners
       ; transpose note-event
       ((note-event engraver event)
        (cond-transp engraver (ly:event-property event 'music-cause)))
       ; transpose key-signature
       ((key-change-event engraver event)
        (cond-transp engraver (ly:event-property event 'music-cause)))
       )
      )
     ))

autoTranspose = \with {
    % we have to ensure, the key-engraver acts after transposition is done
    \remove "Key_engraver"
    \consists \autoTransposeEngraver
    \consists "Key_engraver"
    % if music and print are equal, do nothing
    % else transpose according to transp (up or down)
    music-concert-pitch = ##t
    print-concert-pitch = ##f
    % TODO: if music is given in instrument-pitch, but shall be printed in concert-pitch,
    %   midi pitch is false - instrumentTransposition should be "turned off" for midi(?)
}
