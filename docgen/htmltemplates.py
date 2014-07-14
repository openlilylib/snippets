#!/usr/bin/env python
# -*- coding: utf-8

from PyQt4 import QtCore, QtGui, QtWebKit
import snippets

detailDocHead = ("<html>\n" +
    "<head>\n" +
    "</head>\n" +
    "<body>\n")

detailDocEnd = "</body>\n</html>"

templates = {
    'snippet-title': "<h1>{}</h1>", 
    'snippet-author': '<p class="snippet-author"><span class="field-description">' +
        'Author: </span>{}</p>', 
    'snippet-source': '<p class="snippet-source"><span class="field-description">' +
        'Snippet source or other reference:</span><br />{}</p>', 
    'snippet-short-description': '<p class="snippet-short-description">{}</p>\n', 
    'snippet-description': '<p class="snippet-description">{}</p>\n', 
    'snippet-category': '<p class="snippet-category"><span class="field-description">' +
        'Category: {}</p>', 
    'tags': '<p class="tags"><span class="field-description">Tag: {}</p>', 
    'first-lilypond-version': '<p class="lilypond-version"><span class="field-description">' +
        'First known version: {}</p>', 
    'last-lilypond-version': '<p class="lilypond-version"><span class="field-description">' +
        'Last known version: {}</p>', 
    'status': '<p class="snippet-status"><span class="field-description">' +
        'Status: {}', 
    'snippet-todo': '<p class="snippet-todo"><span class="field-description">' +
        'To do / Bugs / Feature requests:<br />{}', 
    }

listTemplates = {
    'snippet-author': '<p class="snippet-author"><span class="field-description">' +
        'Authors: <ul class="snippet-authors">{}</ul></p>', 
    'tags': '<p class="tags"><span class="field-description">' +
        'Tags: <ul class="tags">{}</ul></p>', 
    'status': '<p class="snippet-status"><span class="field-description">' +
        'Snippet Status: <ul class="snippet-status">{}</ul></p>'
    }

def itemList(fieldName, content):
    lst = ""
    for line in content:
        lst += '<li class="' + fieldName + '">{}</li>'.format(line)
    return listTemplates[fieldName].format(lst)

def headerFieldDoc(snippet, fieldName):
    content = snippet.definition.headerFields[fieldName]
    if isinstance(content, list):
        return itemList(fieldName, content)
    else:
        if content is None:
            return ""
        return templates[fieldName].format(content.replace('\n\n', '</p><p>'))
