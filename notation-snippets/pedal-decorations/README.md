# Pedal Decorations

This code provides a callback function that provides the ability to add arbitrary user specified cautionary text added to the start of a pedal line at the commencement of a new line, and optionally continuation arrow indicators at the end of lines.

### Usage
 The callback is named <code>pedal-with-arrows-and-text</code>.

It must be used by being inserted with <code>before-line-breaking</code>, not <code>after-line-breaking</code>. The text specified is arbitrary, and specified by the user. Passing a boolean in the second argument turns the line ending arrow on or off. Here is an example of the usage:

    \override PianoPedalBracket.before-line-breaking = #(pedal-with-arrows-and-text "(ped)" #t)

This code obviously can only be used with:

    pedalSustainStyle = #'bracket

### Notes
This code has been rewritten to fully support the use of pedal decorations when there is more than one simultaneous pedal line, as occurs in various contemporary scores.

### Author
Andrew Bernard

andrew.bernard@gmail.com

### Acknowledgments
Andrew Bernard originally wrote the code, but it has undergone a very substantial rewrite by Thomas Morley, to whom many thanks are due.
