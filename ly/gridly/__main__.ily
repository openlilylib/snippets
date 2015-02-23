\version "2.18.2"

%% gridly - simple segmented grid for LilyPond
%% Copyright (C) 2015 - Matteo Ceccarello
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%                                Gridly
%%%                                ======
%%%
%%% A simple "segmented grid" framework.
%%%
%%% For documentation take a look at the README and at the contents of
%%% the `example` folder.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(use-modules (oop goops))
#(use-modules (ice-9 regex))

#(define-class <cell> ()
   (music #:init-keyword #:music
          #:getter cell:music)
   (lyrics #:init-keyword #:lyrics
           #:getter cell:lyrics)
   (opening #:init-keyword #:opening
            #:getter cell:opening)
   (closing #:init-keyword #:closing
            #:getter cell:closing))

%%% Some utility functions

#(define (check-coords part segment)
   (cond
    ;; Check segment
    ((not (integer? segment))
     (ly:error "Segment must be an integer, was ~a" segment))
    ((> 1 segment)
     (ly:error "Segment must be > 1, was" segment))
    ((< (hash-ref music-grid-meta #:segments) segment)
     (ly:error "Segment must be less than ~a, was ~a"
               (hash-ref music-grid-meta #:segments) segment))
    ;; Check part
    ((not (string? part))
     (ly:error "Part must be a string"))
    ((not (member part (hash-ref music-grid-meta #:parts)))
     (ly:error "Part must be defined in \\gridInit: ~a" part))
    (#t #t)))

#(define (check-grid)
   (if (and music-grid music-grid-meta)
       #t
       (ly:error "You must first call \\initMusicGrid")))

#(define (display-spaces num-spaces)
   (for-each (lambda (x) (display " ")) (iota num-spaces)))

#(define (get-music-cell part segment)
   (check-coords part segment)
   (hash-ref music-grid (cons part segment)))

#(define (check-durations segment strict)
   (let* ((durations (map
                      (lambda (part)
                        (let ((cell (get-music-cell part segment)))
                          (cons part
                                (if cell
                                    (ly:moment-main (ly:music-length
                                                     (cell:music cell)))
                                    #f))))
                      (hash-ref music-grid-meta #:parts)))
          (defined-durations (filter cdr durations))
          (reference-duration (if (null? defined-durations)
                                  #f
                                  (cdar defined-durations))))
     (if reference-duration
         (for-each
          (lambda (d-pair)
            (if (not (equal? reference-duration (cdr d-pair)))
                (let ((msg-args
                       (list "Expected length of ~a for ~a:~a, got ~a"
                             reference-duration (car d-pair) segment (cdr d-pair))))
                  (if strict
                      (apply ly:error msg-args)
                      (apply ly:warning msg-args)))))
          defined-durations))))

gridDisplay =
#(define-void-function
   (parser location) ()
   (let* ((num-segments (hash-ref music-grid-meta #:segments))
          (segments (map (lambda (x) (+ 1 x)) (iota num-segments)))
          (parts (hash-ref music-grid-meta #:parts)))
     (newline)
     (display "=== Music grid ===")
     (newline)
     (let ((longest-name (reduce max 0
                                 (map string-length parts)))
           (table-spacing (reduce max 0
                                  (map (lambda (seg)
                                         (string-length (number->string seg)))
                                       segments))))
       (display-spaces longest-name)
       (for-each
        (lambda (x)
          (let ((seg-str (number->string x)))
            (display-spaces (+ 1 (- table-spacing (string-length seg-str))))
            (display seg-str)))
        segments)
       (for-each
        (lambda (part)
          (newline)
          (display part)
          (display-spaces (- longest-name (string-length part)))
          (for-each
           (lambda (seg)
             ;(display-spaces (string-length (number->string seg)))
             (display-spaces table-spacing)
             (if (hash-ref music-grid (cons part seg))
                 (display "o")
                 (display "-")))
           segments))
        parts))
     (newline)
     (newline)))

gridCheck =
#(define-void-function
   (parser location) ()
   (for-each
    (lambda (segment)
      (check-durations segment #f))
    (map (lambda (x) (+ 1 x))
         (iota (hash-ref music-grid-meta #:segments)))))

%%% This is taken from Lalily
#(define (test-location? parser location)
   (let ((outname (ly:parser-output-name parser))
         (locname (car (ly:input-file-line-char-column location))))
     (regexp-match? (string-match (format "^(.*/)?~A\\.i?ly$" outname) locname))))

%%% Grid initialization
gridInit =
#(define-void-function
   (parser location segments parts) (number? list?)
   (if music-grid
       (ly:debug "Music grid already initialized, skipping initialization")
       (set! music-grid (make-hash-table)))
   (if music-grid-meta
       (ly:debug
        "Music grid metadata already initialized, skipping initialization")
       (begin
         (set! music-grid-meta (make-hash-table))
         (hash-set! music-grid-meta #:segments segments)
         (hash-set! music-grid-meta #:parts (cons "<template>" parts)))))


%%% Grid manipulation

#(define (context-mod->alist ctx-mod)
   (let ((props '()))
     (if ctx-mod
         (for-each
          (lambda (mod)
            (set! props
                  (assoc-set! props
                              (cadr mod) (caddr mod))))
          (ly:get-context-mods ctx-mod)))
     props))

gridPutMusic =
#(define-void-function
   (parser location part segment ctx-mod music)
   (string? number? (ly:context-mod?) ly:music?)
   (check-grid)
   (check-coords part segment)
   (let* ((props (context-mod->alist ctx-mod))
          (key (cons part segment))
          ;; This closure will look in the `props' alist for the given
          ;; symbol, returning the associated value. If the symbol is
          ;; not in the alist, then a default value is looked up in
          ;; the corresponding `<template>' segment. If even there a
          ;; default value is not found, `default'
          (props-get (lambda (sym last-default)
                       (let ((res (assoc-ref props sym)))
                         (if res
                             res
                             (let ((cell-template
                                    (get-music-cell "<template>" segment)))
                               (if cell-template
                                   (slot-ref cell-template sym)
                                   last-default))))))
          (value (make <cell>
                   #:music music
                   #:lyrics (props-get 'lyrics #f)
                   #:opening (props-get 'opening #{ #})
                   #:closing (props-get 'closing #{ #}))))
     (hash-set! music-grid key value)))

gridSetSegmentTemplate =
#(define-void-function
   (parser location segment ctx-mod music)
   (number? (ly:context-mod? #{ \with{} #}) ly:music?)
   (if (get-music-cell "<template>" segment)
       (ly:debug "Skipping setting of <template>:~a, already set" segment)
       #{
         \gridPutMusic "<template>" $segment $ctx-mod $music
       #}))


#(define (segment-selector? x)
   (or (pair? x)
       (integer? x)
       (equal? 'all x)))

#(define (get-cell-range part start-end)
   (check-grid)
   (let ((start (cond ((equal? 'all start-end)
                       1)
                      ((pair? start-end)
                       (car start-end))
                      ((integer? start-end)
                       start-end)))
         (end (cond ((equal? 'all start-end)
                     (hash-ref music-grid-meta #:segments))
                    ((pair? start-end)
                     (cdr start-end))
                    ((integer? start-end)
                     start-end))))
     (check-coords part start)
     (check-coords part end)
     (let* ((segments (map (lambda (x) (+ x start)) (iota (+ 1 (- end start)))))
            (elems
             (map (lambda (i)
                    (let ((cell (get-music-cell part i)))
                      (cond
                       ;; The cell is defined an populated with music
                       (cell cell)
                       ;; The cell is not defined, but its template is
                       ;; defined. Hence we use the default values provided
                       ;; by the template, except for the lyrics, since
                       ;; there are no notes in this dummy cell.
                       ((get-music-cell "<template>" i)
                        (make <cell>
                          #:lyrics #{ #}
                          #:opening (cell:opening
                                     (get-music-cell "<template>" i))
                          #:closing (cell:closing
                                     (get-music-cell "<template>" i))
                          #:music (cell:music
                                   (get-music-cell "<template>" i))))
                       ;; Neither the cell nor the template are
                       ;; defined. Throw an error.
                       (#t (ly:error
                            "Segment '~a' of part '~a' is still empty and its template is not defined"
                            i part)))))
                  segments)))
       elems)))

gridSetRange =
#(define-void-function
    (parser location start-end) (segment-selector?)
    #{ \setOption gridly.segment-range #start-end #})

gridGetMusic =
#(define-music-function
   (parser location part) (string? )
   (let* ((cells (get-cell-range part #{ \getOption gridly.segment-range #}))
          (music (map cell:music cells))
          (opening (list (cell:opening (car cells))))
          (closing (list (cell:closing (car (last-pair cells))))))
     (make-music
      'SequentialMusic
      'elements (append opening music closing))))

gridGetLyrics =
#(define-music-function
   (parser location part) (string?)
   (let* ((cells (get-cell-range part #{ \getOption gridly.segment-range #}))
          (lyrics (map cell:lyrics cells)))
     (if (member #f lyrics)
         (ly:error "A segment is missing lyrics!")
         (make-music
          'SequentialMusic
          'elements lyrics))))

gridCompileCell =
#(define-void-function
   (parser location part segment)
   (string? number?)
   (check-grid)
   (check-coords part segment)
   (if (test-location? parser location)
       (begin
         (display "Compiling test file\n")
         (if (not (get-music-cell part segment))
             (ly:error "There is no music cell for ~a:~a"
                       part segment))
         (check-durations segment #f)
         #{ \setOption gridly.segment-range $segment #}
         (let* ((name (ly:format "~a-~a" part segment))
                (opening (cell:opening (get-music-cell part segment)))
                (closing (cell:closing (get-music-cell part segment)))
                (selector (cons segment segment))
                (lyrics (let ((maybe-lyrics (cell:lyrics
                                             (get-music-cell part segment))))
                          (if maybe-lyrics
                              #{ \new Lyrics \lyricsto $name $maybe-lyrics #}
                              #{ #})))
                (book
                 #{
                    \book {
                      \score {
                         <<
                           \new Staff \new Voice = $name {
                             $opening
                             \gridGetMusic $part
                             $closing
                           }
                           $lyrics
                         >>
                         \midi{}
                         \layout{}
                      }
                    }
                  #}))
           (ly:book-process book
                            #{ \paper {} #}
                            #{ \layout {} #}
                            (ly:parser-output-name parser))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Deprecated functions

gridTest =
#(define-void-function
   (parser location part segment)
   (string? number?)
   (ly:input-warning
    location
    (string-append
     "\n\tFunction `~a' is deprecated in favor of `~a' and"
     "\n\twill be removed in a future release."
     "\n\tPlease replace the former with the latter.")
    "gridTest" "gridCompileCell")
   ((ly:music-function-extract gridCompileCell) parser location part segment))


gridSetStructure =
#(define-void-function
   (parser location segment ctx-mod music)
   (number? (ly:context-mod? #{ \with{} #}) ly:music?)
   (ly:input-warning
    location
    (string-append
     "\n\tFunction `~a' is deprecated in favor of `~a' and"
     "\n\twill be removed in a future release."
     "\n\tPlease replace the former with the latter.")
    "gridSetStructure" "gridSetSegmentTemplate")
   ((ly:music-function-extract gridSetSegmentTemplate)
    parser location segment ctx-mod music))


gridGetStructure =
#(define-music-function
   (parser location) ()
   (ly:input-warning
    location
    (string-append
     "\n\tThe function `gridGetStructure' is deprecated and is"
     "\n\tno longer part of the public interface of GridLY."
     "\n\tIt will be removed in a future release."
     "\n\tIf you are using this function to retrieve marks and"
     "\n\tand tempo changes, please put them in a dedicated part,"
     "\n\tnamed for instance `marks'"))
   #{
     \gridGetMusic "<template>"
   #})
