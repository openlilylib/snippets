%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of ScholarLY,                                             %
%                      =========                                              %
% a toolkit library for scholarly work with GNU LilyPond and LaTeX.           %
%                                                                             %
% ScholarLY is free software: you can redistribute it and/or modify           %
% it under the terms of the GNU Lesser General Public License as published by %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% ScholarLY is distributed in the hope that it will be useful,                %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU Lesser General Public License for more details.                         %
%                                                                             %
% You should have received a copy of the GNU Lesser General Public License    %
% along with ScholarLY.  If not, see <http://www.gnu.org/licenses/>.          %
%                                                                             %
% ScholarLY is maintained by Urs Liska, ul@openlilylib.org                    %
% Copyright Urs Liska, 2015                                                   %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%{
  Helper utilities to format annotation output
%}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% General routines for formatting output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define string representations for selected ly:music? data types.
% These are used for displaying custom properties.
#(define (format-ly-music music)
   (if (ly:music? music)
       (cond
        ((eq? 'TimeSignatureMusic (ly:music-property music 'name))
              (format "\\time ~a/~a"
                (ly:music-property music 'numerator)
                (ly:music-property music 'denominator)))
        ((eq? 'KeyChangeEvent (ly:music-property music 'name))
         (format "Key: ~a" (ly:music-property music 'tonic)))
         (else "<LilyPond Music>"))
       "No music found"))


% Compose a message from the properties of an annotation
% The 'cmd' argument should be
% - ly:message or
% - append-message
% flt is a list of property names that should *not* be rendered
#(define (format-property-message prop)
   (let
    ((prop-key (string->symbol (car prop)))
     (prop-value (cdr prop)))
    (format "    ~a: ~a"
      (or #{ \getChildOptionWithFallback scholarly.annotate.property-labels #prop-key ##f #}
          prop-key)
      ;; display a placeholder for music expressions
      ;; because these are cluttering the output.
      ;
      ; TODO
      ; maybe improve handling in the future
      ; and format messages for supported types like key signatures
      ;
      (if (ly:music? prop-value)
          (format-ly-music prop-value)
          prop-value))))

#(define (format-property-messages ann flt)
   (map (lambda (prop)
          (if (not (member (car prop) flt))
              (format-property-message prop)
              ""))
     (sort ann
       (lambda (a b)
         (string<? (car a) (car b))))))

