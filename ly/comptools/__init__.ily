



\declareLibrary CompTools \with {
  maintainers = "Urs Liska <ul@openlilylib.org>"
  version = "0.1.0"
  
  short-description = "Tools providing support for different compilation modes."
  description = "This library supports ways to conditionally compile documents,
that is, it provides commands that affect the appearance of the score but that
don't belong to the musical content of the engraved piece. Rather it is at the
level of presentation or of authoring workflows."
}

% Shared variable that can hold any number of break sets.
% Selecting one set to apply makes it possible to manage different
% break sets, e.g. corresponding to different manuscripts
\registerOption comptools.break-sets #'()

% Register a named set of breaks that can be referenced later
#(define registerBreakSet
   (define-void-function (parser location name)
   (symbol?)
   (let ((base-path `(comptools break-sets ,name)))
     #{ \setChildOption #base-path #'line-breaks #'() #}
     #{ \setChildOption #base-path #'page-breaks #'() #}
     #{ \setChildOption #base-path #'page-turns #'() #})))

#(define setConditionalBreaks
(define-void-function (parser location set type breaks)
   (symbol? symbol? list?)
   #{ \setChildOption #`(comptools break-sets ,set) #type #breaks #}))

