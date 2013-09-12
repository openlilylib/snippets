This is a sample snippet markdown.

Snippet image:
![scaling-stencils](scaling-stencils.png)

Snippet code, highlighted by Frescobaldi's exporter written by Urs:

<html>
<pre>
<span style="font-weight: 600;">{</span>
  <span style="">&lt;</span><span style="">e</span><span style="">'</span> <span style="">g</span><span style="">'</span><span style="">&gt;</span><span style="color: #008080;">4</span><span style="color: #008080;">.</span>
  <span style="font-weight: 600;">\once</span> <span style="font-weight: 600;">\override</span> <span style="">Flag</span> <span style="color: #a04900;">#</span><span style="color: #a04900;">'</span><span style="color: #a04900;">stencil</span> =
  <span style="color: #a04900;">#</span><span style="color: #a04900;">(</span><span style="color: #a04900;">lambda</span> <span style="color: #a04900;">(</span><span style="color: #a04900;">grob</span><span style="color: #a04900;">)</span>
     <span style="color: #a04900;">(</span><span style="color: #a04900;">ly:stencil-scale</span> <span style="color: #a04900;">(</span>ly:flag::print grob<span style="color: #a04900;">)</span> <span style="color: #808000;">1</span> <span style="color: #808000;">0.8</span><span style="color: #a04900;">)</span><span style="color: #a04900;">)</span>
  <span style="">q</span><span style="color: #008080;">8</span><span style="color: #c000c0; font-weight: 600;">~</span><span style="">q</span><span style="color: #008080;">8</span> <span style="">q</span><span style="color: #008080;">4</span><span style="color: #008080;">.</span>
<span style="font-weight: 600;">}</span>
<span style="color: #006000; font-weight: 600;">\addlyrics</span> <span style="font-weight: 600;">{</span>
  <span style="color: #006000;">long</span>
  <span style="font-weight: 600;">\override</span> <span style="color: #c000c0;">LyricText</span> <span style="color: #a04900;">#</span><span style="color: #a04900;">'</span><span style="color: #a04900;">stencil</span> =
  <span style="color: #a04900;">#</span><span style="color: #a04900;">(</span><span style="color: #a04900;">lambda</span> <span style="color: #a04900;">(</span><span style="color: #a04900;">grob</span><span style="color: #a04900;">)</span>
     <span style="color: #a04900;">(</span><span style="color: #a04900;">ly:stencil-scale</span> <span style="color: #a04900;">(</span>lyric-text::print grob<span style="color: #a04900;">)</span> <span style="color: #808000;">0.8</span> <span style="color: #808000;">1</span><span style="color: #a04900;">)</span><span style="color: #a04900;">)</span>
  <span style="color: #006000;">long</span>
  <span style="color: #006000;">long</span>
<span style="font-weight: 600;">}</span>

</pre>
</html>
