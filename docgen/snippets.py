#!/usr/bin/env python
# -*- coding: utf-8

import os
import re
from PyQt4 import QtCore

import __main__

class SnippetFile(QtCore.QObject):
    """Snippet file (both definition and usage example.
    Has a filename and a filecontent field
    and an abstract parseFile() method."""
    def __init__(self, owner, filename):
        super(SnippetFile, self).__init__()
        self.owner = owner
        self.filename = filename
        self.version = None
        self.filecontent = None
        self.headercontent = []
        self.bodycontent = []
        try:
            f = open(self.filename)
            self.filecontent = f.readlines()
        finally:
            f.close()
        self.parseFile()
    
    def checkVersion(self, line):
        result = None
        if line.strip().startswith('\\version'):
            result = self.getFieldString(line)
        return result
        
    def getFieldString(self, line):
        """Return the part of 'line' between quotation marks if present."""
        result = re.search('\"(.*)\"', line)
        if result:
            result = result.group(1)
        return result
        
    def parseFile(self):
        raise Exception("SnippetFile.parseFile() has to be " +
                        "implemented in subclasses")
    
    def tagList(self, tagstring):
        """Return a list of tags stripped from whitespace.
        Argument is a comma-separated list."""
        return [ t.strip() for t in tagstring.split(',')]

class SnippetDefinition(SnippetFile):
    """Definition of a snippet"""
    def __init__(self, owner, filename):
        # Define expected header fields
        # Fields that are still None after parsing
        # have not been defined in the snippet
        self.headerFields = {
            'snippet-title': None,
            'snippet-short-description': None,
            'snippet-author': None,
            'snippet-source': None,
            'snippet-description': None,
            'snippet-category': None,
            'tags': None,
            'status': None,
            'first-lilypond-version': None,
            'last-lilypond-version': None,
            'snippet-todo': None }
            
        super(SnippetDefinition, self).__init__(owner, filename)
    
    def parseFile(self):
        i = 0
        while i < len(self.filecontent):
            line = self.filecontent[i]
            # Check for version string
            if not self.version:
                self.version = self.checkVersion(line)
            
            if line.strip().startswith('\\header'):
                # Get the content of the \header section.
                # ATTENTION: The section is considered finished when
                # a line is encountered that has a '}' as its first character
                # and no more content (except whitespace) after that.
                i += 1
                while not self.filecontent[i].rstrip() == '}':
                    self.headercontent.append(self.filecontent[i])
                    i += 1
                
                # After the header the first line containing anything
                # except whitespace and comments is considered as starting
                # the snippet body.
                i += 1
                while ((self.filecontent[i].strip() == '') 
                            or self.filecontent[i].strip().startswith('%')):
                    i += 1
                self.bodycontent = self.filecontent[i:]
                break
                
            # this is only executed until a header is found.
            i += 1
        
        self.parseHeader()

    def parseHeader(self):
        i = 0
        # read in fields
        while i < len(self.headercontent):
            i = self.readField(i)
        # handle the comma-separated-list fields
        self.splitFields(['snippet-author', 'tags'])
        # add snippet to lists for browsing by type
        self.owner.addToAuthors(self.headerFields['snippet-author'])
        self.owner.addToCategory(self.headerFields['snippet-category'])
        self.owner.addToTags(self.headerFields['tags'])

    def readField(self, i):
        while not re.search('(.*) =', self.headercontent[i]):
            i +=1
        line = self.headercontent[i].strip()
        fieldName = line[:line.find('=')-1].strip()
        fieldContent = self.getFieldString(line)
        if not fieldContent:
            fieldContent = [line[line.find('=')+2:].strip()]
            i += 1
            while not self.headercontent[i].strip() == '}':
                fieldContent.append(self.headercontent[i].strip())
                i += 1
            
        self.headerFields[fieldName] = fieldContent
        i += 1
        return i

    def splitFields(self, fields):
        """Split fields that are given as comma-separated lists
        into Python lists."""
        for f in fields:
            self.headerFields[f] = self.tagList(self.headerFields[f])
        
class SnippetExample(SnippetFile):
    """Usage example for a snippet"""
    def __init__(self, owner, filename):
        super(SnippetExample, self).__init__(owner, filename)

    def parseFile(self):
        #TODO: parse the example file
        pass

class Snippet(QtCore.QObject):
    """Object representing a single snippet.
    Contains a definition and an example object."""
    def __init__(self, owner, name):
        super(Snippet, self).__init__()
        self.owner = owner
        self.name = name
        defFilename = os.path.join(__main__.appInfo.defPath, name) + '.ily'
        self.definition = SnippetDefinition(self, defFilename)
        self.example = None

    def addExample(self):
        """Read an additional usage-example."""
        xmpFilename = os.path.join(__main__.appInfo.xmpPath, self.name) + '.ly'
        self.example = SnippetExample(self, xmpFilename)
    
    def addToAuthors(self, authors):
        self.owner.addTo(self.owner.authors, self.name, authors)
        
    def addToCategory(self, catname):
        self.owner.addTo(self.owner.categories, self.name, catname)
    
    def addToTags(self, tags):
        self.owner.addTo(self.owner.tags, self.name, tags)
        
    def hasExample(self):
        """return true if an example is defined."""
        return True if (self.example is not None) else False

class Snippets(QtCore.QObject):
    """Object holding a dictionary of snippets"""
    def __init__(self):
        super(Snippets, self).__init__()
        self.initLists()

    def addTo(self, target, snippet, entry):
        if isinstance(entry, list):
            for e in entry:
                self.addToTarget(target, snippet, e)
        else:
            self.addToTarget(target, snippet, entry)
    
    def addToTarget(self, target, snippet, entry):
        if not target.get(entry):
            target[entry] = []
            target['names'].append(entry)
            target['names'].sort()
        target[entry].append(snippet)
        target[entry].sort()

    def byName(self, name):
        """Return a Snippets object if it is defined."""
        return self.snippets.get(name, None)
        
    def initLists(self):
        self.snippets = {}
        self.names = []
        self.categories = {'names': []}
        self.tags = {'names': []}
        self.authors = {'names': []}
    
    def missingExamples(self):
        result = []
        for d in self.names:
            if not self.snippets[d].hasExample():
                result.append(d)
        return result
    
    def read(self):
        """Read in all snippets and their examples."""
        self.initLists()
        self.names = self.readDirectory(__main__.appInfo.defPath, ['.ily'])
        xmps = self.readDirectory(__main__.appInfo.xmpPath, ['.ly'])
        
        # read all snippets
        for d in self.names:
            self.snippets[d] = Snippet(self, d)
        # read all examples, ignore missing ones
        for x in xmps:
            self.snippets[x].addExample()

    def readDirectory(self, dir, exts = []):
        """Read in the given dir and return a sorted list with
        all entries matching the given exts filter"""
        result = []
        for item in os.listdir(dir):
            (file, ext) = os.path.splitext(item) 
            if ext in exts:
                result.append(file)
        result.sort()
        return result

    # TEMPORARY
    # Create lists of the different items
    # to be used in preliminary visualization
    def displayCategories(self):
        numcats = ' (' + str(len(self.categories)) + ')'
        result = ['Categories' + numcats, '==========', '']
        for c in self.categories['names']:
            result.append(c + ' (' + str(len(self.categories[c])) + ')')
            for i in self.categories[c]:
                result.append('- ' + i)
            result.append('')
        return result

    def displaySnippets(self):        
        numsnippets = ' (' + str(len(self.snippets) - 
                                 len(self.missingExamples())) + ')' 
        result = ['Snippets' + numsnippets, '========', '']
        for s in self.names:
            if self.byName(s).hasExample():
                result.append('- ' + s)
        return result

    def displayTags(self):
        numtags = ' (' + str(len(self.tags['names'])) + ')'
        result = ['Tags' + numtags,  '====', '']
        for t in self.tags['names']:
            result.append(t + ' (' + str(len(self.tags[t])) + ')')
            for i in self.tags[t]:
                result.append('- ' + i)
            result.append('')
        return result
        
