\version "2.17.3"

% TODO: check if everything should be kept. add oll/snippets headers, add example.
% see how this relates to `fried-layout.ily` and possibly merge them.
% update syntax to 2.18


%{
  This file is part of ulLibrary (Urs Liska's LilyPond Toolbox)

  Default Page layout for a classical lied score
  as developed during the Fried Lieder edition.
  It allows for four systems on a page,
  is sufficiently small to get enough music on the page
  while giving a nicely 'bold' impression

  ATM (May 2012) it is

  }%}

% Make it quite small
#(set-global-staff-size 17)
% We'll tweak the style quite a lot because
% with this size it looks quite 'thin'

\paper {
  #(set-paper-size "a4")

  % Page layout
  two-sided = ##t
  top-margin = 12\mm
  bottom-margin = 6\mm
  inner-margin = 18\mm
  outer-margin = 16\mm
  ragged-bottom = ##f
  ragged-last-bottom = ##f

  % Score layout
  indent = 12\mm
  top-markup-spacing #'minimum-distance = #6
  top-system-spacing #'padding = #3
  markup-system-spacing #'padding = #2
  markup-system-spacing #'minimum-distance = #13
  last-bottom-spacing #'padding = #6

  print-first-page-number = ##t
}

\layout {
  % use David Nalesnik's Tupet number corrections
  \correctTupletNumberDefault

  % in 3/4, don't beam x4. x8[ x x]
  \set Timing.beamHalfMeasure = ##f

  \context {
    \Score
    % Add double barlines before time signature change
    \consists \DbBars % (defined in /ulLibrary/layout/DbBars_engraver.ily)

    % Visual tweaks to create the 'bold' appearance

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % General sizes
    % Increase fontSize to make it 'bolder'
    fontSize = #0.4

    % Adjust Clef and TimeSignature so they aren't out of place

    % TimeSignature just reverts the fontSize
    % so it fits between the staff lines
    % TimeSignature is therefore smaller than the music
    % but we decided it doesn't hurt
    \override TimeSignature #'font-size = #-0.4

    % Reverting Clef size completely looks strange
    % Therefore #-0.25 is a compromise
    \override Clef #'font-size = #-0.25

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Adjustments

    % Now we'll have to adjust many items
    % so they produce a coherent style
    % (generally everything becomes thicker)

    % line thicknesses
    \override Hairpin #'thickness = 1.5
    \override Stem #'thickness = 1.7
    \override BarLine #'hair-thickness = 2
    \override Slur #'line-thickness = 1
    \override Glissando #'thickness = #1.4

    % Make staffSwitch line and Glissando thicker
    \override VoiceFollower #'thickness = #1.3
    \override TextSpanner #'thickness = #1.75

    % adjust Ottava Bracket and Text
    \override OttavaBracket #'font-size = #0.3
    \override OttavaBracket #'thickness = #1.3

    % Make Arpeggio slightly thinner
    \override Arpeggio #'font-size = #-0.25

    % Adjust the dash structure of text spanners
    \override TextSpanner #'dash-period = #3
    \override TextSpanner #'dash-fraction = #0.2

    % Adjust the dash structure of StaffSwitch and Glissando
    % Only effective when #'dashed-line style
    \override VoiceFollower #'dash-period = #2
    \override VoiceFollower #'dash-fraction = #0.6

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Font settings

    % set all Fonts to Adobe Minion Pro (through the \include above)

    % Adjust the font size of the different score items

    \override TupletNumber #'font-size = #-1
    \override BarNumber #'font-size = #1.25
    \override LyricText #'font-size = #1.25

    % Just there as a reminder of earlier experiments
    % Might prove useful again, but rather not
    \override TextScript #'font-size = #0.5
    \override TextSpanner #'font-size = #0.5
    \override DynamicTextSpanner #'font-size = #0.5
    \override MultiMeasureRestText #'font-size = #0.5

    % Place and resize Metronome marks (and tempo Strings)
    \override MetronomeMark #'padding = #2
    \override MetronomeMark #'font-size = #2
  }

  \context {
    \PianoStaff
    connectArpeggios = ##t
    \accidentalStyle "piano"
    \override InstrumentName #'font-size = #1.25
    % \consists #Span_stem_engraver
  }

  \context {
    \Staff
    \override InstrumentName #'font-size = #1.25
  }

  \context {
    \Dynamics
    \override TextScript #'font-size = #1.2 % has to be set separately (a bug?)
    \override TextSpanner #'font-size = #1.2
    \override DynamicTextSpanner #'font-size = #1.2
    \consists "Bar_engraver"
    \consists "Separating_line_group_engraver"
    \override BarLine #'transparent = ##t
  }

  \context {
    \Voice
    % Correct positioning of staccato dots when they are parenthesized or other articulations are present
    \consists \StaccatoCorrector % (defined in /ulLibrary/layout/staccato_corrector_engraver.ily)
  }

  \context {
    \Lyrics
    % expand hyphen's mininal distance, so there are always hyphens
    % force some wider spacing instead of removing the hyphen
    \override LyricHyphen #'minimum-distance = #0.6
  }

}
