\version "2.16.2" % absolutely necessary!

\header {
  snippet-title = "Annotate grobs"
  snippet-author = "Urs Liska"
  snippet-description = \markup {
    Add annotations to arbitrary grobs in an input file.
    These annotations can be used to 
    - issue compiler messages,
    - write annotations to external files,
    - color annotated grobs,
    - possibly print annotations on top of the score.
    
    The use of context mods as argument makes it very straightforward
    to enter annotations, see the enclosed example file for how
    it works.
    
    Calling syntax:
    "\annotate \with { [list of expressions] } grobname"
    If you leave out the grobname the function will implicitly
    annotate the next notehead.
    You can also use postfix notation to annotate individual
    grobs like with the "\tweak" command.
    
  }
  status = "started"
  todo = \markup {
    Currently this is only a stub allowing for entering annotations
    without causing compiler warnings. As a placeholder affected grobs
    are printed magenta. Any further functionality has to be implemented yet.
    
    - Determine current musical moment as well as current position
      in the source file for point-and-click links.
    - There should be a set of predefined annotation types (e.g.
      "todo", "critical remark", "discussion" etc.). These may behave
      differently, e.g. color the output differently or export information
      differently formatted.
    - Apart from that, arbitrary annotation types can be entered with some
      predefined response, e.g. output sorted lists for each detected type.
    
    - Find a way to "enclose" images as links (maybe kind of Markdown style?)
    - Find a way to insert score examples in the message.)
      This could either be done by including LilyPond code (would be best)
      or by linking to external source files and/or image/pdf scores.
    
    - For further discussion see 
      https://github.com/openlilylib/lilypond-doc/wiki/Documenting-musical-content
  }

  % add comma-separated tags to make searching more effective:
  tags = "documentation, annotations, editorial tools"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

\version "2.16.2"

#(define (props-alist l)
   ;; takes the list of context-mods and creates
   ;; an alist with type/content pairs
   (cond ((null? l)
          '())
          (else
           (cons (cdr (car l)) (props-alist (cdr l))))))
      
#(define (iter-props p)
   ; TODO:
     ; Make this conditional and maybe improve formatting
     ; Use this also to store the annotation (in an object?)
   (cond ((null? p)
     '())
     (else
      (ly:message "   ~a: ~s\n" (caar p) (cadar p))
      (newline)
      (iter-props (cdr p)))))

#(define (symbol-or-music? item)
   ;; new predicate:
   ;; I don't want to annotate grob lists
   (cond ((symbol? item) #t)
     ((ly:music? item) #t)
     (else #f)))

annotate = 
#(define-music-function (parser location properties item)
   (ly:context-mod? symbol-or-music?)
   ;; annotates a musical object for use with lilypond-doc
   (let ((props (props-alist (ly:get-context-mods properties))))

     ; Plan/TODO:
       ; set defaults (e.g. for type and format)
       ; check against a list of accepted types
         ; respond to it
           ; (e.g. set configuration variables,
           ;  choose a different output file etd.)
         ; if it's no accepted type choose a default action
       ; iterate over props (existing iter-props)
       ; for each prop do
         
       ; keep in mind to respect configuration variables:
         ; output to console
         ; output to file
         ; coloring
         ; etc.

     ; some debug/test actions
     (ly:message "Annotation:\n")
     (iter-props props)
     
     ;(write props)
     ;(newline)
     
     ; TODO: Trying to set defaults, doesn't work yet
     ; the else clause doesn't work yet!
     ;(cond ((assoc 'type props)(newline))
     ;  (else ((cons ('type "annotation") props))))
     ;(newline)
     ;(display props)
     

     ; Dummy coloring
     #{ 
       \once \tweak color #magenta #item
     #}
   ))
