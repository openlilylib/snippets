% Logging for openLilyLib libraries

% Define loglevel constants
#(define oll-loglevel-nolog    0)
#(define oll-loglevel-critical 1)
#(define oll-loglevel-warning  2)
#(define oll-loglevel-log      3)
#(define oll-loglevel-debug    4)

% Set default loglevel of warning
#(define oll-loglevel oll-loglevel-warning)
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
   (if (>= oll-loglevel oll-loglevel-critical)
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
   (if (>= oll-loglevel oll-loglevel-warning)
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
   (if (>= oll-loglevel oll-loglevel-log)
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
   (if (>= oll-loglevel oll-loglevel-debug)
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
