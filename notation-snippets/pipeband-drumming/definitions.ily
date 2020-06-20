\version "2.16.2"
\header {
  snippet-title = "Pipeband Drumming Staff"
  snippet-author = "Eric Teunis de Boone"
  snippet-author-email = "ericteunis@gmail.com"
  snippet-source = ""
  snippet-description = \markup {
	Add (left,right) hand definitions to the drumPitchTable.
	This code provides a new "PipebandDrumStaff".

	As music, it provides shortcuts for flourishes, embellishments(such as flams/drags/..) and special sticking.
	There was an effort made for midi output
  }
  tags = "pipeband, drumming, lilydrum"
  status = "ready"
}

% Note name defs :
%	"d" for the right hand ("droite") and
%	"g" for the left hand ("gauche")
%-----------------------------------------

#(ly:set-option 'point-and-click #f)
#(ly:set-option 'relative-includes #t)

\include "./includes/layout_tweaks.ily" % Code for the Staff
\include "./includes/musical_functions.ily" %
\include "./includes/embellishments.ily"
\include "./includes/flourishing.ily"
\include "./includes/special_sticking.ily"
\include "./includes/midi.ily"
