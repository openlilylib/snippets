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
    
  }
  status = "started"
  todo = \markup {
    Currently this is only a stub allowing for entering annotations
    without causing compiler warnings. As a placeholder affected grobs
    are printed magenta. Any further functionality has to be implemented yet.
    
    - The coloring through tweak doesn't seem to be "\once" currently.
    
    - What is lacking is any kind of checking against a list of predefined
      message types, formats etc.
    
    - Find a way to "enclose" images as links (maybe kind of Markdown style?)
    - Find a way to insert score examples in the message.
    
    - For further discussion see 
      https://github.com/openlilylib/lilypond-doc/wiki/Documenting-musical-content
  }

  % add comma-separated tags to make searching more effective:
  tags = "documentation, annotations, editorial tools"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

\version "2.17.18"

annotate = 
#(define-music-function (parser location properties item)
   (ly:context-mod? symbol-list-or-music?)
   ;; annotates a musical object for use with lilypond-doc
   
   ; Dummy coloring
     #{ 
       \once \tweak color #magenta #item
     #}
   )

