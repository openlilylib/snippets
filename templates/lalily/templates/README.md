
predefined lalily-templates
===========================

In this folder are predefined templates placed, which are loaded automatically, if you `\include "templates/lalily/definitions.ily"`
Every file ending with `.ily` is included by definitions.ily

* lalily.instrument
    the base template for any instruments.
    
    music: #'() = current path
    
    options:
	* name : the Staff-name (not the display name)
	* init-voice : any musical statements to initialize the voice [{}]
	* clef : the used clef ["G"]
	* transposition : transposition of this instrument
	* naturalize : (t/_f_) pitches shall be naturalized
	* input-concert-pitch : (_t_/f) music is coded in concert pitch
	* output-concert-pitch : (t/_f_) music shall be displayed in concert pitch
	* staff-mods : context-modifications added to the Staff
	* voice-mods : context-modifications added to the Voice
	* midi-instrument : midi instrument to use
	* meta : name of "meta"-track - may for example be renamed to "global"
	   TODO: other templates
* lalily.instrument.group
    template to group multiple instruments.
    
    music: per a-list entry (TODO example)
    
    options:
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

* lalily.piano
    piano template
    
    music:
      * right: right hand
      * left: left hand
      * dynamics: centered dynamics (optional)
      * pedal: dynamics context for pedal (optional)
      
    options:
      * context-mods: PianoStaff context-mods
      * right-mods: right Staff context-mods
      * left-mods: left Staff context-mods
      * dynamic-mods
      * pedal-mods
      * right-clef ["G"]
      * left-clef ["bass"]

* lalily.vocal
    voice template
    
    music:
      * music: melody/notes
      * lyrics: lyrics
    
    options:
      * clef ["G"]
      * vocname: Voice context name (for lyricsto) MANDATORY
      * staffname: Staff context name
      * staff-mods
      * voice-mods
      * lyric-mods
      * repeats: volta repeats for lyrics (TODO not implemented yet, example)
      * verses: lyric verses
* lalily.vocal.group
    voice group template
    
    music: per a-list entry (staffs)
    
    options:
      staff-mods: Staff context-modifications for all Staffs
      group-mods: StaffGroup context-modifications
      mensur: (t/_f_) true for no BarLine but SpanBars
      staffs: a-list with Staff options (TODO structure reference and example)

