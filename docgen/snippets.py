#!/usr/bin/env python
# -*- coding: utf-8

import os
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
        f = open(self.filename)
        self.filecontent = f.readlines()
        f.close()
        self.parseFile()
    
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
        super(SnippetDefinition, self).__init__(owner, filename)
    
    def parseFile(self):
        #TODO: parse the definition file
        #TEMPORARY!!!
        self.readCategoryTags()
        
    def readCategoryTags(self):
        #TODO: Implement this correctly.
        # for now it serves only to get a list of used categories
        for line in self.filecontent:
            line = line.strip()
            if line.startswith("snippet-author"):
                self.owner.addToAuthors(self.tagList(line[line.find('\"')+1:-1]))
            if line.startswith("snippet-category"):
                self.owner.addToCategory(line[line.find('\"')+1:-1])
            if line.startswith("tags"):
                self.owner.addToTags(self.tagList(line[line.find('\"')+1:-1]))
        
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
        self.owner.addToAuthors(self.name, authors)
        
    def addToCategory(self, catname):
        self.owner.addToCategory(self.name, catname)
    
    def addToTags(self, tags):
        self.owner.addToTags(self.name, tags)
        
    def hasExample(self):
        """return true if an example is defined."""
        return True if (self.example is not None) else False

class Snippets(QtCore.QObject):
    """Object holding a dictionary of snippets"""
    def __init__(self):
        super(Snippets, self).__init__()
        self.initLists()

    def addToAuthors(self, name, authors):
        for a in authors:
            if not self.authors.get(a):
                self.authors[a] = []
                self.authornames.append(a)
                self.authornames.sort()
            self.authors[a].append(name)
            self.authors[a].sort()
    
    def addToCategory(self, name, category):
        if not self.categories.get(category):
            self.categories[category] = []
            self.catnames.append(category)
            self.catnames.sort()
        self.categories[category].append(name)
        self.categories[category].sort()
        
    def addToTags(self, name, tagstoadd):
        for t in tagstoadd:
            if not self.tags.get(t):
                self.tags[t] = []
                self.tagnames.append(t)
                self.tagnames.sort()
            self.tags[t].append(name)
            self.tags[t].sort()
        
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
        for c in self.catnames:
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
        numtags = ' (' + str(len(self.tagnames)) + ')'
        result = ['Tags' + numtags,  '====', '']
        for t in self.tagnames:
            result.append(t + ' (' + str(len(self.tags[t])) + ')')
            for i in self.tags[t]:
                result.append('- ' + i)
            result.append('')
        return result
        
