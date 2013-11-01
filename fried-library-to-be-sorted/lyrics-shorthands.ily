\version "2.17.3"
% /includes/ulLibrary/tweaks/lyrics.ily

%{
  This library is part of ulLibrary (Urs Liska's LilyPond Toolbox)
  and may also be included separately.
  It assumes that the project root contains any number of
  first level subdirectories containing the to-be-compiled .ly files
  and a folder /includes/ulLibrary.
  So this file is always accessible through the relative path "../includes/ulLibrary/curvesToolbox.ily

  the lyrics toolbox contains
  - shorthands to align lyrics
  - functions to control melisma behaviour
%}



%%%%%%%%%%%%%%
% Align lyrics

lyrAlign =
#(define-music-function (parser location amount)
   (number?)
   #{
     \once \override LyricText #'self-alignment-X = #amount
   #})

lyrLeftI =      \lyrAlign #0.2
lyrLeftII =     \lyrAlign #0.4
lyrLeftIII =    \lyrAlign #0.6
lyrLeftIIII =   \lyrAlign #0.8
lyrLeft =       \lyrAlign #1

lyrRightI =     \lyrAlign #-0.2
lyrRightII =    \lyrAlign #-0.4
lyrRightIII =   \lyrAlign #-0.6
lyrRightIIII =  \lyrAlign #-0.8
lyrRight =      \lyrAlign #-1


%{Set melisma behaviour
  Some composers (e.g. Oskar Fried) don't use slurs and beams
  accordings to the standards, so one has to deal with melismaBusyProperties
  - for which the two commands offer shorthands.
  Generally melOn should be active vor vocal melodies,
  so ties, slurs and beams cause a melisma.
  If the lyrics proceed under ties, slurs or beams,
  melOff has to be activated.
  The command has to be placed after the beginning of the syllable
%}
melOff = {
  \set melismaBusyProperties = #'()
}
melOn = {
  \unset melismaBusyProperties
}
