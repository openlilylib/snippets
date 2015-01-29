% Logging for openLilyLib libraries

% Define loglevel constants
#(define oll-loglevel-nolog 0)
#(define oll-loglevel-critical 1)
#(define oll-loglevel-warning 2)
#(define oll-loglevel-log 3)
#(define oll-loglevel-debug 4)

% TODO: Make the loglevel configurable
% when the general configuration is designed
#(define oll-loglevel oll-loglevel-warning)

% Open output file where everything will be logged
#(define oll-logfile
   (if (> oll-loglevel 0)
       (begin
        (open-output-file
         (format  "~a.oll.log" (ly:parser-output-name parser))))
        #f))
#(if oll-logfile
     (format oll-logfile "openLilyLib logfile for ~a\n" (ly:parser-output-name parser)))


% Different logging levels can be output.
% Can be used with or without location argument

% Critical error
#(define (oll:error location fmt . vals)
   (if (>= oll-loglevel oll-loglevel-critical)
       (if (ly:input-location? location)
           (begin
            (ly:error location (format fmt vals))
            (format oll-logfile fmt vals))
           (begin
            (ly:error (format location fmt))
            (format oll-logfile
              (format "openLilyLib error: ~a\n" location) fmt)))))

% Warning
#(define (oll:warn location fmt . vals)
   (if (>= oll-loglevel oll-loglevel-warning)
       (if (ly:input-location? location)
           (begin
            (ly:input-warning location (format fmt vals))
            (format oll-logfile fmt vals))
           (begin
            (ly:warning (format location fmt))
            (format oll-logfile
              (format "openLilyLib warn:: ~a\n" location) fmt)))))

% Logging
#(define (oll:log location fmt . vals)
   (if (>= oll-loglevel oll-loglevel-log)
       (if (ly:input-location? location)
           (begin
            (ly:input-message location (format fmt vals))
            (format oll-logfile fmt vals))
           (begin
            (ly:message (format location fmt))
            (format oll-logfile
              (format "openLilyLib log: ~a\n" location) fmt)))))
