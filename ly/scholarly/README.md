# ScholarLY

*ScholarLY* is a toolbox for scholarly editions with (mainly) GNU LilyPond.
It belongs to *openLilyLib*, LilyPond's community library system.
Therefore getting and installing *ScholarLY* is automatically handled by
preparing *openLilyLib*.
See its [home page](https://github.com/openlilylib/openlilylib) for details.

As part of *openLilyLib* this library is released under the
GNU General Public License. See *openLilyLib*'s license for details.

## Overview

*ScholarLY* will include a number of features, but currently only `\annotate` is implemented,
and this can only be seen as a beginning. See the `\annotate`'s [overview](annotate/README.md)
for more information on this functionality.

## Getting Started

Prerequisite to using *ScholarLY* is activating *openLilyLib* with

```lilypond
\include "openlilylib"
```

*openLilyLib* will only be initialized once so it is safe to use this command in multiple
initialization files. Loading ScholarLY is done with one of:

```lilypond
\loadModule "scholarly"
\loadModule "scholarly/annotate"
```

Once it is running *Scholarly*'s behaviour can be configured using *openLilyLib*'s
global configuration mechanism. Please refer to the manuals of both for more information.
