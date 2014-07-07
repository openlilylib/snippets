\version "2.18.0"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generic

\registerTemplate generic
#(define-music-function (parser location piece options)(list? list?)
   (get-music piece location))

\registerTemplate NOTFOUND
#(define-music-function (parser location piece options)(list? list?)
   (ly:input-message location "No template specified for [~A]!" (glue-list piece "."))
   (get-music piece location))

% mirror another music-folder
% needs option 'mirror-path
% may set other options fo the inherited templated
\registerTemplate lalily.mirror
#(define-music-function (parser location piece options)(list? list?)
   (let ((path (assoc-get 'mirror-path options #f #f)))
     (if (not (list? path))
         (begin
          (ly:input-warning location "no mirror-path! (~A | ~A)" path piece)
          (set! path '(..))
          ))
     #{
       \createScoreWithOptions #path #options
     #}))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% init contexts

\registerTemplate lalily.init.Voice
#(define-music-function (parser location piece options)(list? list?)
   (let* ((localsym (assoc-get 'init-path options '(init) #f))
          (deepsym (assoc-get 'deepsym options 'init #f))
          (deepdef (assoc-get 'default options #{ #}))
          (deepm #{ \getMusicDeep $deepdef #deepsym #}))
     #{
       \getMusic $deepm $localsym
     #}))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% create a group
\registerTemplate lalily.group
#(define-music-function (parser location piece options)(list? list?)
   (let* ((elms (assoc-get 'part options (assoc-get 'element options '())))
          (group (assoc-get 'group options #f))
          (group-mods (assoc-get 'group-mods options #f))
          (parts (if (> (length elms) 0)
                     (make-music 'SimultaneousMusic 'elements
                       (map
                        (lambda (p)
                          (let* ((opts (cdr p))
                                 (template (assoc-get 'template opts '(generic)))
                                 (path (assoc-get 'music opts (list (car p))))
                                 )
                            #{ \callTemplate ##t #template #path #opts #}
                            )) elms))
                     (make-music 'SimultaneousMusic 'void #t))))
     (if (symbol? group)
         #{
           \new $group \with {
             $(if (ly:context-mod? group-mods) group-mods)
             \consists \editionEngraver $piece
           } $parts
         #}
         parts
         )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Transpose

\registerTemplate lalily.transpose
#(define-music-function (parser location piece options)(list? list?)
   (let ((template (ly:assoc-get 'template options #f #f))
         (opts (let ((pce (ly:assoc-get 'piece options #f #f))) (if pce (get-default-options pce location) options)))
         (pce (ly:assoc-get 'piece options piece #f))
         (pdiff (ly:assoc-get 'transpose options piece #f) )
         )
     (if (not (list? pce))(set! pce (list pce)))
     (ly:music-transpose (ly:music-deep-copy
                          (call-template template parser location pce options)
                          ) pdiff)
     ))
setTransposedTemplate =
#(define-void-function (parser location t1 t2 piece tmpl options)
   (ly:pitch? ly:pitch? list? list? list?)
   (set-default-template piece '(lalily transpose)
     (assoc-set-all! options
       `((transpose . ,(ly:pitch-diff t2 t1))
         (template . ,tmpl)))))


