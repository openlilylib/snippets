
predefined lalily-templates
===========================

In this folder are predefined templates placed, which are loaded automatically, if you `\include "templates/lalily/definitions.ily"`
Every file ending with `.ily` is included by definitions.ily

* lalily.instrument
    the base template for any instruments. It takes options:
	* name : the Staff-name (not the display name)
	* init-voice : any musical statements to initialize the voice
	* clef : the used clef
	* transposition : transposition of this instrument
	* naturalize : (t/f) pitches shall be naturalized
	* input-concert-pitch : (t/f) music is coded in concert pitch
	* output-concert-pitch : (t/f) music shall be displayed in concert pitch
	* staff-mods : context-modifications added to the Staff
	* voice-mods : context-modifications added to the Voice
	* midi-instrument : midi instrument to use
	* meta : name of "meta"-track - may for example be renamed to "global"
* lalily.instrument.group
    template to group multiple instruments. options:
	* groupmod : context-modifications added to the StaffGroup
	* staffs : association list with staff-options
	   TODO more info on staff-structure

predefined instruments (more to come)

* lalily.instrument.wood.oboe
* lalily.instrument.wood.english-horn
* lalily.instrument.sax.sop
* lalily.instrument.sax.alt
* lalily.instrument.sax.ten
* lalily.instrument.sax.bar
* lalily.instrument.brass.trumpet
* lalily.instrument.brass.trombone
* lalily.instrument.strings.violin
* lalily.instrument.strings.viola
* lalily.instrument.strings.cello

