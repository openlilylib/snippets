\version "2.16.2" % absolutely necessary!

\header {
  snippet-title = "Ignore Collision(s)"
  snippet-author = "Urs Liska"
  snippet-description = \markup {
    LilyPond issues warnings when two or more notes share the same
    note column. However, sometimes it is necessary to do that,
    for example when doing some polyphony tricks.
    It is useful to suppress the warnings in this case so the
    - expected - warnings don't "hide" the real ones.
  }
  status = "ready"
  tags = "shorthand, notecolumn, collision, clash"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%	
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%


% Suppress a warning about clashing note columns
% Use when you deliberately use clashing note columns 
% e.g. when merging voices to one stem
% or when hiding voices (although for tying the
% \hideVoiceForTie approach is recommended)
% 
ignoreCollision = \once \override NoteColumn #'ignore-collision = ##t

% Use this one with care, as it will prevent lilypond from 
% warning you about real problems
ignoreCollisions = \override NoteColumn #'ignore-collision = ##t
warnOnCollisions = \override NoteColumn #'ignore-collision = ##f
