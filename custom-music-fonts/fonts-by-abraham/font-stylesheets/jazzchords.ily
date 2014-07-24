\version "2.18.2"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creating jazz-style chords 
% Borrowed from Jean-Pierre's good work at:
%               https://sites.google.com/site/jpgzic/home
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% modification of the procedure "chordRootNamer"
%---- Definition of chord alterations -------
#(define (JazzChordNames pitch majmin)	;majmin is a required argument for "chordNamer", but not used here
  (let* ((alt (ly:pitch-alteration pitch)))
    (make-line-markup
      (list
	(make-simple-markup 
	  (vector-ref #("C" "D" "E" "F" "G" "A" "B")
	    (ly:pitch-notename pitch)))
	(if (= alt 0)			; alteration ?
	  (markup "")		; do nothing
	  (if (= alt FLAT)	; flat or sharp
	    (markup ">")
	    (markup "<")
	  )
	)
      )
    )
  )
)

%----- markup commands to make it easier to write chords -----
%----- this section can be adapted as desired (I don't like "MI" and "MA", for example)
% for minor chords, use "acMin" to print a small "m"
#(define-markup-command (acMin layout props extension) (string?)
  (interpret-markup layout props
    (markup #:small "m" #:super extension)))

% for major chords, use "acMaj" to print a small "M"
#(define-markup-command (acMaj layout props extension) (string?)
  (interpret-markup layout props
    (markup #:super "M" #:super extension)))

% for chords with up to three alterations, stacked on top of each other
#(define-markup-command (acAlt layout props strA strB strC) (string? string? string?)
  (interpret-markup layout props
    (markup 
      #:super strA
      #:fontsize 1.5 "["
      #:fontsize -4 
      #:raise 1.8
      #:column (strB strC) 
      #:fontsize 1.5 "]"
    )
  )
)

% modify the built-in "chordNameExceptions"
%----- change chord exceptions for jazz notation -----
% 
%	degrés 			markup				% écriture
JazzChordsList = {
% minor third chords - 3 notes
  <c es ges>1-\markup { \super "dim." } % :dim
  <c es g>-\markup { \acMin #"" } % :m

% minor third chords - 4 notes
  <c es ges beses>-\markup { \super "7dim" } % :dim7
  <c es gis>-\markup { \acMin #"aug" } % :m5+ (Ab/C)	
  <c es g a>-\markup { \acMin #"6" } % :m6            
  <c es ges bes>-\markup { \acMin #"7 >5" } % :m7.5-         
  <c es g bes>-\markup { \acMin #"7" } % :m7            
  <c es gis bes>-\markup { \acMin #"7 <5" } % :m7.5+         
  <c es g b>-\markup { \acMin #"M7" } % :m7+           
  <c es g d'>-\markup { \acMin #"add9" } % :m5.9
	
% minor third chords - 5+ notes
  <c es g a d'>-\markup { \acMin #"6/9" } % :m6.9
  <c es g bes des'>-\markup { \acMin #"7(>9)" } % :m7.9-
  <c es g bes d'>-\markup { \acMin #"9" } % :m9
  <c es ges bes d'>-\markup { \acMin #"9(>5)" } % :m9.5-
  <c es g b d'>-\markup { \acMin #"9(M7)" } % :m9.7+
  <c es g bes dis'>-\markup { \acMin #"7(<9)" } % :m7.9+
  <c es g bes f'>-\markup { \acMin #"7(add 11)" } % :m7.11
  <c es g bes a'>-\markup { \acMin #"7(add 13)" } % :m7.13
  <c es g bes d' f'>-\markup { \acMin #"11" } % :m11
  <c es ges bes d' f'>-\markup { \acMin #"11(>5)" } % :m11.5-
  <c es g bes d' f' a'>-\markup { \acMin #"13" } % :m13

% major third chords - 3 notes
  <c e ges>-\markup { \super ">5" } % :5-
  <c e gis>-\markup { \super "aug" } % :aug

% major third chords - 4 notes
  <c e g a d'>-\markup {
	  \super "6"
	  \hspace #-0.5 \raise #0.7 \tiny "/"
	  \hspace #-0.5 \tiny "9"
	  }	% :6.9
% special chords
  <c e g bes c'>-\markup { \super "7(Alt)" } % :c:8
  <c e g b>-\markup { \acMaj #"7" } % :maj
  <c e ges b>-\markup { \acMaj #"7>5" } % :maj.5-
  <c e gis b>-\markup { \acMaj #"7<5" } % :maj.5+
	
  <c e ges bes>-\markup { \super "7(>5)" } % :7.5-
  <c e gis bes>-\markup { \super "7(<5)" } % :7.5+
  <c e g d'>-\markup { \super "add9" } % :5.9

% major third chords - 5+ notes
  <c e g b d'>-\markup { \acMaj #"9" } % :maj9
  <c e g bes des'>-\markup { \super "7(>9)" } % :9-
  <c e ges bes des'>-\markup { \acAlt #"7" #">9" #">5" } % :9-.5-
  <c e gis bes des'>-\markup { \acAlt #"7" #">9" #"<5" } % :9-.5+
  <c e g bes d' fis'>-\markup { \super "9(<11)" } % :9.11+
  <c e g bes dis'>-\markup { \super "7(<9)" } % :9+
  <c e ges bes dis'>-\markup { \acAlt #"7" #"<9" #">5" } % :9+.5-
  <c e gis bes dis'>-\markup { \acAlt #"7" #"<9" #"<5" } % :9+.5+
  <c e g bes fis'>-\markup { \super "7(<11)" } % :7.11+
  <c e g bes aes'>-\markup { \super "7(>13)" } % :7.13-
  <c e g b d' f'>-\markup { \acMaj #"11" } % :maj11
  <c e g b d' a' >-\markup { \acMaj #"13" } % :maj13 
  <c e g bes d' a' >-\markup { \super "13" } % :13 
  <c e g bes des' a' >-\markup { \super "13(>9)" } % :13.9-

% sustained chords
  <c d g>-\markup { \super "sus2" } % :sus2
  <c f g>-\markup { \super "sus" } % :sus4
  <c f g bes>-\markup { \super "7sus" }  % :sus4.7
  <c f g bes d'>-\markup { \super "9sus" }  % :sus4.7.9
  <c f g dis'>-\markup { \super "<9sus" }  % :sus4.9+
}

% variable needed to use chord exceptions
JazzChords = #(append (sequential-music-to-chord-exceptions JazzChordsList #t) ignatzekExceptions)

% modify the default ChordNames context
\layout {
  \context { 
    \ChordNames
    chordRootNamer = #JazzChordNames	% update the chord names
    chordNameExceptions = #JazzChords	% update the chord exceptions
    \override ChordName.font-name = #"lilyjazzchord"  % use the custom font for displaying the chords
    \override ChordName.font-size = #2
  }
}

