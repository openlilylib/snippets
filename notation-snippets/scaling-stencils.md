This is a sample snippet markdown.

Snippet image:
![scaling-stencils](scaling-stencils.png)

Snippet code, highlighted by Frescobaldi's exporter written by Urs:

<html>
<head>
<meta http-equiv='Content-Type' content='text/html; charset=utf-8'>
<style type="text/css">body {
  color: #3c3c3c;
  background: #ffffff;
}

.html-attrname {
  color: #0000ff;
}

.html-comment {
  color: #808080;
  font-style: italic;
}

.html-entityref {
  color: #008080;
}

.html-lilypondtag {
  color: #0000c0;
  font-weight: 600;
}

.html-string {
  color: #c00000;
}

.html-tag {
  font-weight: 600;
}

.html-value {
  color: #808000;
}

.lilypond-articulation {
  color: #ff8000;
  font-weight: 600;
}

.lilypond-chord {
  
}

.lilypond-chorditem {
  
}

.lilypond-command {
  color: #0000c0;
  font-weight: 600;
}

.lilypond-comment {
  color: #808080;
  font-style: italic;
}

.lilypond-contextname {
  color: #c000c0;
  font-weight: 600;
}

.lilypond-contextproperty {
  color: #0000ff;
}

.lilypond-delimiter {
  font-weight: 600;
}

.lilypond-duration {
  color: #008080;
}

.lilypond-dynamic {
  color: #ff8000;
  font-weight: 600;
}

.lilypond-error {
  color: #ff0000;
}

.lilypond-fingering {
  color: #ff8000;
}

.lilypond-grobname {
  color: #c000c0;
}

.lilypond-keyword {
  font-weight: 600;
}

.lilypond-lyric {
  color: #006000;
}

.lilypond-lyricmode {
  color: #006000;
  font-weight: 600;
}

.lilypond-lyrictie {
  color: #c000c0;
  font-weight: 600;
}

.lilypond-markup {
  color: #008000;
  font-weight: 400;
}

.lilypond-note {
  
}

.lilypond-octave {
  
}

.lilypond-octavecheck {
  
}

.lilypond-pipesymbol {
  
}

.lilypond-repeat {
  color: #0000c0;
  font-weight: 600;
}

.lilypond-rest {
  
}

.lilypond-schemestart {
  color: #a04900;
}

.lilypond-skip {
  
}

.lilypond-slur {
  color: #c000c0;
  font-weight: 600;
}

.lilypond-specifier {
  color: #0000ff;
}

.lilypond-string {
  color: #c00000;
}

.lilypond-stringnumber {
  color: #ff8000;
}

.lilypond-stringquoteescape {
  color: #008080;
}

.lilypond-tremolo {
  color: #0000c0;
  font-weight: 600;
}

.lilypond-usercommand {
  color: #0000ff;
}

.lilypond-uservariable {
  
}

.lilypond-value {
  color: #808000;
}

.lilypond-variable {
  color: #0000ff;
}

.scheme-closeparen {
  color: #a00096;
  font-weight: 600;
}

.scheme-comment {
  color: #808080;
  font-style: italic;
}

.scheme-constant {
  color: #9600de;
}

.scheme-function {
  color: #9600de;
}

.scheme-keyword {
  color: #008296;
  font-weight: 600;
}

.scheme-lilypond {
  color: #a04900;
  font-weight: 600;
}

.scheme-number {
  color: #808000;
}

.scheme-openparen {
  color: #a00096;
  font-weight: 600;
}

.scheme-scheme {
  color: #a04900;
}

.scheme-string {
  color: #c00000;
}

.scheme-symbol {
  color: #9600de;
}

.scheme-variable {
  color: #9600de;
}

.texinfo-attribute {
  color: #0000ff;
}

.texinfo-block {
  color: #0000c0;
  font-weight: 600;
}

.texinfo-comment {
  color: #808080;
  font-style: italic;
}

.texinfo-escapechar {
  color: #008080;
}

.texinfo-keyword {
  font-weight: 600;
}

.texinfo-verbatim {
  color: #c00000;
}
</style>
</head>
<body>
<pre><span class="lilypond-delimiter">{</span>
  <span class="lilypond-chord">&lt;</span><span class="lilypond-note">e</span><span class="lilypond-octave">'</span> <span class="lilypond-note">g</span><span class="lilypond-octave">'</span><span class="lilypond-chord">&gt;</span><span class="lilypond-duration">4</span><span class="lilypond-duration">.</span>
  <span class="lilypond-keyword">\once</span> <span class="lilypond-keyword">\override</span> <span class="lilypond-uservariable">Flag</span> <span class="lilypond-schemestart">#</span><span class="scheme-scheme">'</span><span class="scheme-symbol">stencil</span> =
  <span class="lilypond-schemestart">#</span><span class="scheme-openparen">(</span><span class="scheme-keyword">lambda</span> <span class="scheme-openparen">(</span><span class="scheme-scheme">grob</span><span class="scheme-closeparen">)</span>
     <span class="scheme-openparen">(</span><span class="scheme-function">ly:stencil-scale</span> <span class="scheme-openparen">(</span><span class="scheme-function">ly:flag::print</span> <span class="scheme-scheme">grob</span><span class="scheme-closeparen">)</span> <span class="scheme-number">1</span> <span class="scheme-number">0.8</span><span class="scheme-closeparen">)</span><span class="scheme-closeparen">)</span>
  <span class="lilypond-note">q</span><span class="lilypond-duration">8</span><span class="lilypond-slur">~</span><span class="lilypond-note">q</span><span class="lilypond-duration">8</span> <span class="lilypond-note">q</span><span class="lilypond-duration">4</span><span class="lilypond-duration">.</span>
<span class="lilypond-delimiter">}</span>
<span class="lilypond-lyricmode">\addlyrics</span> <span class="lilypond-delimiter">{</span>
  <span class="lilypond-lyric">long</span>
  <span class="lilypond-keyword">\override</span> <span class="lilypond-grobname">LyricText</span> <span class="lilypond-schemestart">#</span><span class="scheme-scheme">'</span><span class="scheme-symbol">stencil</span> =
  <span class="lilypond-schemestart">#</span><span class="scheme-openparen">(</span><span class="scheme-keyword">lambda</span> <span class="scheme-openparen">(</span><span class="scheme-scheme">grob</span><span class="scheme-closeparen">)</span>
     <span class="scheme-openparen">(</span><span class="scheme-function">ly:stencil-scale</span> <span class="scheme-openparen">(</span><span class="scheme-function">lyric-text::print</span> <span class="scheme-scheme">grob</span><span class="scheme-closeparen">)</span> <span class="scheme-number">0.8</span> <span class="scheme-number">1</span><span class="scheme-closeparen">)</span><span class="scheme-closeparen">)</span>
  <span class="lilypond-lyric">long</span>
  <span class="lilypond-lyric">long</span>
<span class="lilypond-delimiter">}</span>
</pre>
</body>
</html>
