% Usage examples for \annotate

\version "2.19.0"

% Real-world include command:
% \include "editorial-tools/annotate/definitions.ily"
\include "definitions.ily"

\header {
  title = "Annotate grobs"
  subtitle = "Currently this only colors them magenta"
}

\relative g' {
  % default call with specified grob
  \annotate \with {
    type = "critical-remark"
    context = "vc1"
    author = "Urs Liska"
    date = "2013-06-06"
    format = "plain"
    message = "Tenuto added 
    as in Vc. 2"
  }
  Script
  g1-- -\markup "Explicitly annotate Script" |
  
  % default call without specified grob (defaults to NoteHead (?))
  \annotate \with {
    type = "question"
    source = "MS2"
    message = "Ms. 2: b flat"
  }
  a4 ~  ^\markup "Implicitly annotate notehead, tie is not affected" a b 
  
     % postfix call
     c-\annotate \with {
      type = "todo-engraving"
      message = "Improve tie engraving"
    }
    ~ -\markup "Postfix annotation"
    c 
    d ~ d e-- 
}