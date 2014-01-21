Including Xelatex
=================

The provided command provides a markup-list-command to add one EPS-stencil per page of the resulting PDF file.
This might be used for other programs, creating PDF files.

The command produces a bash-script "xelatex-<tmpname>.sh", which actually creates the PDF.
The given tex-source MUST be a document body, meaning the text between

    \begin{document}

and

    \end{document}

The template is given by the commmand to supply the right dimensions without margins. That way, the xelatex-markup matches the layout of the rest of the document.

ToDo:
-----

- While calling the xelatex command, lilypond sets a lot of environment variables which affect xelatex and all ghostscript-dependant commmands.
  This leads to a lot of warnings and is *not* really solved.
- It might be better, to use pandoc. Pandoc /can/ use templates for PDF creation and it can read several formats beside latex, like, for example, markdown.

Notes:
-----

- The file is split into EPS-files with `pdftops -eps -f <n> -l <n>`, because `pdf2ps "<name>%d.eps"` rasterizes the PDF-source.

