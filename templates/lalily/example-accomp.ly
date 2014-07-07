\version "2.18.0"
\include "example-satb.ly"
\include "input-shorthands/sizeContext/definitions.ily"

% In this file we use the formerly entered choral piece "Alta Trinita Beata" and
% add a trumpet solo and a piano reduction to it.
% Statement: I used a lovely sacred tune here and added a trumpet solo to it.
%            I don't want to harm anybodies feelings about this! This is for demo purposes only!

% use another global staff size (this has to be done early! - TODO)
#(set-global-staff-size 16)


% prepare a template
\registerTemplate choir-accomp
#(define-music-function (parser location piece options)(list? list?)
   #{
     <<
       % prepare options for trumpet
       \optionsInit acc-trmp
       % add Staff context-modification
       \optionsAdd acc-trmp staff-mods \with {
         % give it a name
         instrumentName = "Trumpet (Bb)"
         % make it a little bit bigger (see input-shorthands/sizeContext/definitions.ily)
         \sizeContext #1
         % remove empty staves
         \RemoveEmptyStaves
       }
       % read music in concert pitch (default)
       \optionsAdd acc-trmp input-concert-pitch ##t
       % write music in instrument transposition (default)
       \optionsAdd acc-trmp output-concert-pitch ##f
       % call trumpet template on path trumpet with options
       \callTemplate LY_ROOT.lalily.instrument.brass.trumpet trumpet #acc-trmp

       % prepare options for modded integration of the choir
       \optionsInit choir
       % all Staff context will receive these modifications
       \optionsAdd choir staff-mods \with { midiInstrument = "choir aahs" \sizeContext #-1 }
       % use registered template and options from upper folder to create choir part
       \createScoreWithOptions LY_UP #choir

       % prepare options for piano part
       \optionsInit acc-pno
       % call piano template on path piano
       \callTemplate LY_ROOT.lalily.piano piano #acc-pno
     >>
   #})

% -> current path is music.choral.altatrinita, \musicPath accomp returns music.choral.altatrinita.accomp
% now bind template choir-accomp to music-folder music.choral.altatrinita.accomp with empty options
%\setDefaultTemplate \musicPath accomp choir-accomp #'()

% the use of the lalily.group template is much easier than writing a template, so it is used instead
% to achieve the same effect as in the custom template above, we prepare the following options
\optionsInit opts
\optionsAdd opts part.trumpet.template \Path lalily.instrument.brass.trumpet
\optionsAdd opts part.trumpet.staff-mods \with {
         % give it a name
         instrumentName = "Trumpet (Bb)"
         % make it a little bit bigger (see input-shorthands/sizeContext/definitions.ily)
         \sizeContext #1
         % remove empty staves
         \RemoveEmptyStaves
       }
\optionsAdd opts part.choir.template \Path lalily.mirror
\optionsAdd opts part.choir.mirror-path #'(.. ..)
\optionsAdd opts part.choir.staff-mods \with { midiInstrument = "choir aahs" \sizeContext #-1 }
\optionsAdd opts part.piano.template \Path lalily.piano
\setDefaultTemplate \musicPath accomp lalily.group #opts

% inherit all headers (title,composer etc.) from music-folder above = music.choral.altatrinita
\inheritAllHeaders LY_UP
% TODO prepare paper so that music looks at least OK ... now it looks "urg"
\setPaper \paper {
  indent = 22\mm
  ragged-bottom = ##f
  ragged-last-bottom = ##f
}
% speed up midi
\setMidi \midi {
  \tempo 4=120
}

% add some commands to the meta part
\putMusic LY_UP.meta { \numericTimeSignature \getMusic LY_UP.meta }

% compose a funny melody for the trumpet (in concert pitch!)
\putMusic trumpet \relative c'' {
  r4. c8 a4. bes8 | c4 d2 e4 | f8 e d c a4 g | f4.. f'16 f4 e8 d |
  c4. b8~ b bes8 a g | f g a bes c e f d | c a f d c' a g e | f4 f'2 c4 |
  % ... to be continued ... or not ;)
}

% produce piano part part-combining sopranos and altos for the right and tenors and basses for the left hand
% ... this is only a demo - if you want to create a piano-reduction, this will only be the starting-point for the work todo!
% TODO There is a warning about breathing signs in 2.19?
\putMusic piano.global { \set Staff.printPartCombineTexts = ##f }
\putMusic piano.right { \partcombine \getMusic LY_UP.sop.music \getMusic LY_UP.alt.music }
\putMusic piano.left { \partcombine \getMusic LY_UP.ten.music \getMusic LY_UP.bas.music }

% sheet edition-tag is already active from example-input ...
\editionMod sheet 9 0/1 Score.A \break
\editionMod sheet 17 0/1 Score.A \break

% now make annotations
% TODO add lalily-annotations with a list in an extra bookpart
% TODO add lalily-markup-styles
\addEdition anno
\editionMod anno 5 3/8 trumpet.Staff.A ^\markup { \italic \bold \with-color #red ouch! }

% engrave music
\lalilyTest
