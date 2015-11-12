# Tablature

The *Tablature* library is a collection of modules which provide features not
currently supported upstream by LilyPond.  While some of these features
might not be specific of tablatures or instruments like guitar, bass, etc.,
the name Tablature seemed to be the best comprehensive term in most cases.
Below you can read about the available modules.

## Microtones

On a fretted instrument, like guitar (in standard tuning), there are no frets
for microtones, but there are two cases where microtones in tablature
are needed:

- when the tuning contains a microtone
- when a string is bended to produce a microtone

As LilyPond supports microtones in tablature since
[version 2.19.31](https://sourceforge.net/p/testlilyissues/issues/4643/),
this module should be used only to compile files written for earlier versions.

## Bending

(to be added)
