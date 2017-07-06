\include "deprecate.ily"

#(ly:message "general-tools/includeHelper from openLilyLib/snippets is deprecated.
Please use \\includePattern from the oll-core.include-pattern module instead.

")

\ollRedirect
"general-tools/includeHelper/definitions.ily"
"_internal/utilities/include-pattern.ily"