%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of openLilyLib,                                           %
%                      ===========                                            %
% the community library project for GNU LilyPond                              %
% (https://github.com/openlilylib/openlilylib                                 %
%              -----------                                                    %
%                                                                             %
% openLilyLib is free software: you can redistribute it and/or modify         %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% openLilyLib is distributed in the hope that it will be useful,              %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU General Public License for more details.                                %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
%                                                                             %
% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
% and others.                                                                 %
%       Copyright Urs Liska, 2015                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Logging for openLilyLib libraries

% Define loglevel constants
#(define oll-loglevel-nolog    0)
#(define oll-loglevel-critical 1)
#(define oll-loglevel-warning  2)
#(define oll-loglevel-log      3)
#(define oll-loglevel-debug    4)

#(define oll-logfile #f)

% Open a log file when the first entry is actually written
openLogfile =
#(define-void-function (parser location)()
   (if (not oll-logfile)
       (set! oll-logfile
             (open-output-file
              (format "~a.oll.log" (ly:parser-output-name parser))))))


% Different logging levels can be output.
% Can be used with or without location argument

% Critical error
#(define (oll:error location fmt . vals)
   (if (>= #{ \getOption global.loglevel #} oll-loglevel-critical)
       (begin
        ;; open logfile upon first request
        #{ \openLogfile #}
        (if (ly:input-location? location)
            (begin
             ;; console output
             (ly:error location
               (format
                (string-append "openLilyLib: " fmt) vals))
             ;; logfile output
             (format oll-logfile fmt vals))
            (begin
             ;; this is an "abuse" of the parameters,
             ;; "location" is actually the "fmt" argument
             (ly:error
              (format
               (string-append "openLilyLib: " location) fmt))
             (format oll-logfile
               (format "error: ~a\n" location) fmt))))))

% Warning
#(define (oll:warn location fmt . vals)
   (if (>= #{ \getOption global.loglevel #}  oll-loglevel-warning)
       (begin
        #{ \openLogfile #}
        (if (ly:input-location? location)
            (begin
             (ly:input-warning location
              (format
               (string-append "openLilyLib: " fmt) vals))
             (format oll-logfile fmt vals))
            (begin
             (ly:warning
              (format
               (string-append "openLilyLib: " location) fmt))
             (format oll-logfile
               (format "warning: ~a\n" location) fmt))))))

% Logging
#(define (oll:log location fmt . vals)
   (if (>= #{ \getOption global.loglevel #}  oll-loglevel-log)
       (begin
        #{ \openLogfile #}
        (if (ly:input-location? location)
            (begin
             (ly:input-message location
               (format
                (string-append "openLilyLib: " fmt) vals))
             (format oll-logfile fmt vals))
            (begin
             (ly:message
              (format
               (string-append "openLilyLib: " location) fmt))
             (format oll-logfile
               (format "log: ~a\n" location) fmt))))))

% Debug output
#(define (oll:debug location fmt . vals)
   (if (>= #{ \getOption global.loglevel #}  oll-loglevel-debug)
       (begin
        #{ \openLogfile #}
        (if (ly:input-location? location)
            (begin
             (ly:input-message location
               (format
                (string-append "openLilyLib: " fmt) vals))
             (format oll-logfile fmt vals))
            (begin
             (ly:message
              (format
               (string-append "openLilyLib: " location) fmt))
             (format oll-logfile
               (format "log: ~a\n" location) fmt))))))
