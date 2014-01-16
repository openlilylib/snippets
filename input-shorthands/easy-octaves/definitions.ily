\version "2.16.2"

\header {
  snippet-title = "Easy octaves"
  snippet-author = "Tao Cumplido"
  snippet-description = \markup {
    Easily write octavized notes with capitalized note-names.
    Has to be modified for languages other than the default.
  }
  snippet-dedication = "Thanks to David Kastrup for the idea."
  tags = "octavize, octaves, automatic octaves"
  status = "ready"
}

%{
  Info:
  
  This snippet enables capitalized note-names to quickly write
  octavized pitches. It currently supports only the default
  language (dutch) but is easily modified to support other
  languages.
  
  To use this snippet simply include it. It is automatically
  applied to all the music.
  
  Note:
  The snippet won't work with music that needs microtones
  with eigth-tone values.
  The q shorthand will recognize a capital note name as a chord.
  
  
  TODO
  Is there a way to make q treat a capital note-name like a
  regular note?
  
%}

#(define (octavize music)
   (let* ((p (ly:music-property music 'pitch))
          (d (ly:music-property music 'duration))
          (o (ly:pitch-octave p))
          (n (ly:pitch-notename p))
          (a (ly:pitch-alteration p))
          (alt (ly:music-property music 'articulations)))
     (set! music (make-music
                  'EventChord
                  'elements
                  (append
                   (list
                    (make-music
                     'NoteEvent
                     'duration d
                     'pitch p)
                    (make-music
                     'NoteEvent
                     'duration d
                     'pitch (ly:make-pitch (+ 1 o) n a)))
                   alt))))
   music)
 
#(define (check-octaves music)
   (if (eq? (ly:music-property music 'name) 'NoteEvent)
       (if (not (eq? (ly:music-property music 'pitch #f) #f)) ; skip drum notes
           (let* ((p (ly:music-property music 'pitch))
                  (o (ly:pitch-octave p))
                  (n (ly:pitch-notename p))
                  (a (ly:pitch-alteration p)))
             (cond ((= (denominator a) 8)
                    (set! a (- a 1/8))
                    (ly:music-set-property! music 'pitch (ly:make-pitch o n a))
                    (set! music (octavize music)))))))
   music)

addOctaves =
#(define-music-function (parser location music) (ly:music?)
   (music-map (lambda (x) (check-octaves x)) music))

#(set! toplevel-music-functions
       (cons (lambda (music parser) #{ \addOctaves #music #})
         toplevel-music-functions))

#(define language-add-octaves
   (list
    `(nl-octaves . ,(append (assoc-get 'nederlands language-pitch-names #f)
       `(
         (Ceses . ,(ly:make-pitch -1 0 -7/8))
         (Ces . ,(ly:make-pitch -1 0 -3/8))
         (C . ,(ly:make-pitch -1 0 1/8))
         (Cis . ,(ly:make-pitch -1 0 5/8))
         (Cisis . ,(ly:make-pitch -1 0 9/8))
       
         (Deses . ,(ly:make-pitch -1 1 -7/8))
         (Des . ,(ly:make-pitch -1 1 -3/8))
         (D . ,(ly:make-pitch -1 1 1/8))
         (Dis . ,(ly:make-pitch -1 1 5/8))
         (Disis . ,(ly:make-pitch -1 1 9/8))
       
         (Eses . ,(ly:make-pitch -1 2 -7/8))
         (Es . ,(ly:make-pitch -1 2 -3/8))
         (E . ,(ly:make-pitch -1 2 1/8))
         (Eis . ,(ly:make-pitch -1 2 5/8))
         (Eisis . ,(ly:make-pitch -1 2 9/8))
         
         (Feses . ,(ly:make-pitch -1 3 -7/8))
         (Fes . ,(ly:make-pitch -1 3 -3/8))
         (F . ,(ly:make-pitch -1 3 1/8))
         (Fis . ,(ly:make-pitch -1 3 5/8))
         (Fisis . ,(ly:make-pitch -1 3 9/8))
         
         (Geses . ,(ly:make-pitch -1 4 -7/8))
         (Ges . ,(ly:make-pitch -1 4 -3/8))
         (G . ,(ly:make-pitch -1 4 1/8))
         (Gis . ,(ly:make-pitch -1 4 5/8))
         (Gisis . ,(ly:make-pitch -1 4 9/8))
         
         (Ases . ,(ly:make-pitch -1 5 -7/8))
         (As . ,(ly:make-pitch -1 5 -3/8))
         (A . ,(ly:make-pitch -1 5 1/8))
         (Ais . ,(ly:make-pitch -1 5 5/8))
         (Aisis . ,(ly:make-pitch -1 5 9/8))
         
         (Beses . ,(ly:make-pitch -1 6 -7/8))
         (Bes . ,(ly:make-pitch -1 6 -3/8))
         (B . ,(ly:make-pitch -1 6 1/8))
         (Bis . ,(ly:make-pitch -1 6 5/8))
         (Bisis . ,(ly:make-pitch -1 6 9/8))
       
         )))))

#(set! language-pitch-names (append language-pitch-names language-add-octaves))

\language "nl-octaves"