\version "2.19.45"

\include "./definitions.ly"


\paper {
  indent = 0\mm
  ragged-bottom = ##t
}
\layout {
  indent = 30\mm
  \context {
    \Score
    \override NonMusicalPaperColumn #'line-break-permission = ##f
  }
}

\header {
    title = "Cheat sheet for entering pipeband drumming music"
    subtitle = "Intended for Lilypond 2.18 or better"
    tagline = \markup { "ET de Boone, Seaforths of Holland" }
}
\score {
	\new PipeBandDrumStaff {
		\time 2/4
		\drummode {
		  d4 g 
		  <<d g>> r
		  \crossstick d \crossstick g 
		  \rimshot d \rimshot g 
		  \backstick d \backstick g
		}
    }
    \addlyrics {
		"d" "g" 
		"<<d g>>" 
		"\crossstick d" "\crossstick g" 
		"\rimshot d" "\rimshot g" 
		"\backstick d" "\backstick g"
    }
	\header {
	  	title = \markup \column {\line{Plain Notes}}
		subtitle = ""
	}
}

\score {
	\new PipeBandDrumStaff {
	 	\tempo 2/4
		\drummode {
			d^\splitTheFeather g^\cartWheel
			d^\up r4 
			d8 g d4  
			\cart d4 \stf d4
			\flourish { d4 g d g }
		}
    }
    \addlyrics {
		"\splitTheFeather" "\cartWheel" 
		"\up" 
		"d" "g" "d"
		"\cart" "\stf"
		"\flourish {" "d4" "g" "d" "g}"
    }
	\header {
	  	title = \markup \column {\line{Flourishing I}}
		subtitle = ""
	}
}
\score {
	\new PipeBandDrumStaff {
		\drummode {
			d^\lthrow g^\rthrow 
			d^\bthrow r |

			d^\lpush g^\rpush
			d^\bpush r |

			d^\andrewStop d^\quiggs
			d^\stop r |

			d^\blfy d^\rblfy
			r r |
		}
    }
    \addlyrics {
		"\lthrow" "\rthrow" "\bthrow"  
		"\lpush" "\rpush" "\bpush"   
		"\andrewStop" "\quiggs" "\stop"   
		"\blfy" "\rblfy"
    }
	\header {
	  	title = \markup \column {\line{Flourishing II}}
		subtitle = ""
	}
}
\score {
	\new PipeBandDrumStaff {
		\time 2/4
		\drummode {
			\flam d		\flam g
			\drag d		\drag g
			\odrag d	\odrag g
			\ruff d		\ruff g
			\sruff d	\sruff g
		}
	}
    \addlyrics {
		"\flam d"	"\flam g"
		"\drag d"	"\drag g"
		"\odrag d"	"\odrag g"
		"\ruff d"	"\ruff g"
		"\sruff d"	"\sruff g"
	}
	\header {
	  	title = \markup \line{ Embellishments }
		subtitle = ""
	}
}
\score {
	\new PipeBandDrumStaff {
		\time 4/4
		\drummode {
			\flamg d	\flamd g
			\flamg g	\flamd d
			\dragg d	\dragd g
			\dragg g	\dragd d
			\odragg d	\odragd g
			\odragg g	\odragd d
			\break
			\ruffg d	\ruffd g
			\ruffg g	\ruffd d
			\sruffg d	\sruffd g
			\sruffg g	\sruffd d
		}
	}
    \addlyrics {
		"\flamg d"	"\flamd g"
		"\flamg g"	"\flamd d"
		"\dragg d"	"\dragd g"
		"\dragg g"	"\dragd d"
		"\odragg d"	"\odragd g"
		"\odragg g"	"\odragd d"
		"\ruffg d"	"\ruffd g"
		"\ruffg g"	"\ruffd d"
		"\sruffg d"	"\sruffd g"
		"\sruffg g"	"\sruffd d"
    }
	\header {
		title = \markup \column { \line {Embellishments: Forced Hand}}
		subtitle = ""
	}
}
\score {
	\new PipeBandDrumStaff {
	  	\time 4/4
		\eighthBeaming
		\drummode {
			d4:32( d8:32)(-> g8) \flam d4 r8 d8:32( \dr |
			\tuplet 3/2 { g16[) d g } \drag d16. g32]  d32 g d g d16. g32-> d16. d32 \flam g4 s8	
		}
	}
	\addlyrics {
		"d4:32(" "\\triplet " -- "\\flam d16." "g32"  -- "\\drag g16" -- "\\triplet" "g:64" "d-> }"
    }
	\header {
		title = "Rolls"
		subtitle = ""
    }
}



\pageBreak
\score {
	\new PipeBandDrumStaff {
		\drummode {
			\time 6/8
			\repeat volta 2 {
				\partial 8 d8:32(_"7" |
				g8.[) g16 \dragd d8] d8.[ d16 \dragg g8] |
				g8.[ \dragd d16 d8] d8.[ d16 \dragg g8] |
				\flamddr d8. d16 g8 d4.:32(_"13" |
				d8.) g16 d8 \flamg g4 d8:32(_"7" \fr |
				\break
				g8.[) g16 \dragd d8] d8.[ d16 \dragg g8] |
				g8.[ \dragd d16 d8] d8.[ d16 \dragg g8] |
				\flamddr d8. d16:64(_"5" d8) \flamd d8. d16 \flamg g8 |
				d8.:32(_"5" d16) \flamg g8 \flamd d4 \fr  s8 |
			}
		}
	}
	\addlyrics {
		\partial 8
		"d8:32(" "\\triplet " -- "\\flam d16." "g32"  -- "\\drag g16" -- "\\triplet" "g:64" "d-> }"
    }

	\header {
		title = "And now for something completely standard"
		subtitle = ""
	}
}
% # Writing parts
% rolls
% tuplets
% tutti
% accents ( > v ^)
%

