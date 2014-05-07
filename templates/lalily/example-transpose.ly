\version "2.18.0"
\include "example-satb.ly"

% prepare options
\optionsInit opts
\optionsAdd opts mirror-path #'(..)

% we inluded example-satb and the music-folder is still set to music.choral.altatrinita
% \musicPath G sets it to music.choral.altatrinita.G
% the lalily.mirror template creates the music from the given (relative) mirror-path,
% which is here #'(..) -> music.choral.altatrinita
% \setTransposedTemplate wraps the given template in a transposition
\setTransposedTemplate f g \musicPath G lalily.mirror #opts
% inheritAllHeaders gets and sets all headers from given (relative) path
\inheritAllHeaders LY_UP

% engrave it
\lalilyTest
% ... and now we have the included music in a transposed version :)
