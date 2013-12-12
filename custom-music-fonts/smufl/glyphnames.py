"""
Converts the JSON glyph names for SMUFL into a LilyPond include.
"""
import json
import textwrap

IN_FILENAME = 'glyphnames.json'
OUT_FILENAME = 'smufldata.ily'

PREAMBLE = '''%{
Copyright (c) 2013 Steinberg Media Technologies GmbH
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%}'''

content = json.load(open(IN_FILENAME, 'r'))

outfile = open(OUT_FILENAME, 'w')
outfile.write('\n\n'.join([textwrap.fill(par, 70) for par in PREAMBLE.splitlines()]) + '\n' * 3)
outfile.write("#(define smufl-map '(\n")

for name, value in content.items():
    try:
        code = value['codepoint'][2:]
    except KeyError:
        continue
    outfile.write('  ("{}" . #x{})\n'.format(name, code))

outfile.write('))')
outfile.close()
