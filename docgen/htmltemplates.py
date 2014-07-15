#!/usr/bin/env python
# -*- coding: utf-8

from PyQt4 import QtCore, QtGui, QtWebKit
import snippets

detailDocHead = ("<html>\n" +
    "<head>\n" +
    '<link rel="stylesheet" href="css/detailPage.css">\n' +
    "</head>\n" +
    "<body>\n")

detailDocEnd = "</body>\n</html>"

templates = {
    'title': '<div id="title">{}</div>', 
    'meta': '<div id="meta">\n' +
        '<h2>Metadata</h2>{}</div>', 
    'status-div': '<div id="status">\n' +
        '<h2>Status information</h2>\n' +
        '{compat}\n{status}</div>', 
    'definition-body': '<div id="lilypond">\n' +
    '<h2>Snippet definition</h2><pre>{}</pre></div>', 
    'snippet-title': "<h1>{}</h1>", 
    'snippet-author': '<div class="snippet-author"><span class="field-description">' +
        'Author: </span>{}</div>', 
    'snippet-source': '<div class="snippet-source"><span class="field-description">' +
        'Snippet source or other reference:</span><br />{}</div>', 
    'snippet-short-description': '<div class="snippet-short-description">{}</div>\n', 
    'snippet-description': '<div class="snippet-description">{}</div>\n', 
    'snippet-category': '<div class="snippet-category"><span class="field-description">' +
        'Category: </span>{}</div>', 
    'tags': '<div class="tags"><span class="field-description">Tag: {}</div>', 
    'first-lilypond-version': '<div class="lilypond-version"><span class="field-description">' +
        'First known version: </span>{}<br />', 
    'last-lilypond-version': '<span class="field-description">' +
        'Last known version: </span>{}</div>', 
    'status': '<div class="snippet-status"><span class="field-description">' +
        'Snippet Status: </span>{}</div>', 
    'snippet-todo': '<div class="snippet-todo"><span class="field-description">' +
        'To do / Bugs / Feature requests:</span><br />{}</div>',
    'lilypond-code': '<pre class="lilypond">{}</pre>'
    }

listTemplates = {
    'snippet-author': '<div class="snippet-author"><span class="field-description">' +
        'Authors: </span><ul class="snippet-authors">{}</ul></span></div>', 
    'tags': '<div class="tags"><span class="field-description">' +
        'Tags: </span><ul class="tags">{}</ul></div>', 
    'status': '<div class="snippet-status"><span class="field-description">' +
        'Snippet Status: </span><ul class="snippet-status">{}</ul></div>'
    }

# Generic function to generate code
#

def fieldDocs(snippet, fields, default = False):
    result = ''
    for f in fields:
        result += headerFieldDoc(snippet, f, default)
    return result

def itemList(fieldName, content):
    lst = ""
    for line in content:
        lst += '<li class="' + fieldName + '">{}</li>'.format(line)
    return listTemplates[fieldName].format(lst)

def headerFieldDoc(snippet, fieldName, default = False):
    content = snippet.definition.headerFields[fieldName]
    if isinstance(content, list):
        return itemList(fieldName, content)
    else:
        if content is None:
            if not default:
                return ''
            content = "Undefined"
        return templates[fieldName].format(content.replace('\n\n', '</p><p>'))

def hr():
    return "<hr />\n"

# Individual sections

def compatibilityDoc(snippet):
    return fieldDocs(snippet, 
        ['first-lilypond-version', 
         'last-lilypond-version'], True)
    
def definitionBody(snippet):
    return templates['definition-body'].format(
            lilypondToHtml(''.join(snippet.definition.bodycontent)))
    
def metaDoc(snippet):
    return templates['meta'].format(fieldDocs(snippet, 
        ['snippet-source', 
         'snippet-category', 
         'tags']))

def statusDoc(snippet):
    return templates['status-div'].format(
        compat = compatibilityDoc(snippet), 
        status = fieldDocs(snippet, 
            ['status', 
             'snippet-todo']))

def titleDoc(snippet):
    return templates['title'].format(fieldDocs(snippet, 
                ['snippet-title', 
                 'snippet-short-description', 
                 'snippet-author']))

def lilypondToHtml(code):
    """Return formatted LilyPond code as HTML.
    (This is a stub for a to-be-developed function)."""
    
    #TODO: use Frescobaldi's ly-music module
    return templates['lilypond-code'].format(code)

