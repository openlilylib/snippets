ISMN
====

An ISMN has the same syntax as ISBN and the checksum is calculated like in EAN (european article number).

Defined are:

    scheme-lambda: (create-ismn publisher title)
	scheme-function: \createISMN "<pppp>" "<tttt>"
	markup-command: \ismn #"<pppp>" #"<tttt>"

If you need the correct ISMN code for your publications, you can use these functions to create the correct string.

    \createISMN "006" "46415"

yields

    M 006-46415-9

for "Bärenreiter Verlag, Mass in B-Minor".

If you have your own publishing business, you can easily create a function that only takes the title number to produces the ISMN-strings for your titles accordingly.

