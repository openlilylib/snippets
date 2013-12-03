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

annotate = 
#(define-music-function (parser location properties item)
   (ly:context-mod? symbol-list-or-music?)
   ;; annotates a musical object for use with lilypond-doc
   
   ; Dummy coloring
     #{ 
       \once \tweak color #magenta #item
     #}
   )

