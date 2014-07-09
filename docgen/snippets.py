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
        super(SnippetFile, self)
        self.owner = owner
        self.filename = filename
        f = open(self.filename)
        self.filecontent = f.readlines()
        f.close()
        self.parseFile()
    
    def parseFile(self):
        raise Exception("SnippetFile.parseFile() has to be " +
                        "implemented in subclasses")

class SnippetDefinition(SnippetFile):
    """Definition of a snippet"""
    def __init__(self, owner, filename):
        super(SnippetDefinition, self).__init__(owner, filename)
    
    def parseFile(self):
        #TODO: parse the definition file
        #TEMPORARY!!!
        self.owner.addToCategory("Test-Kategorie")
        self.owner.addToTags("erster-tag")
        self.owner.addToTags("zweiter-tag")
        
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
        super(Snippet, self)
        self.owner = owner
        self.name = name
        defFilename = os.path.join(__main__.appInfo.defPath, name) + '.ily'
        self.definition = SnippetDefinition(self, defFilename)
        self.example = None

    def addExample(self):
        """Read an additional usage-example."""
        xmpFilename = os.path.join(__main__.appInfo.xmpPath, self.name) + '.ly'
        self.example = SnippetExample(self, xmpFilename)
    
    def addToCategory(self, catname):
        self.owner.addToCategory(self.name, catname)
    
    def addToTags(self, tagname):
        self.owner.addToTags(self.name, tagname)
        
    def hasExample(self):
        """return true if an example is defined."""
        return True if (self.example is not None) else False

class Snippets(QtCore.QObject):
    """Object holding a dictionary of snippets"""
    def __init__(self):
        self.snippets = {}
        self.names = []
        self.categories = {}
        self.tags = {}

    def addToCategory(self, name, category):
        if not self.categories.get(name):
            self.categories[category] = []
        self.categories[category].append(name)
        
    def addToTags(self, name, tag):
        if not self.tags.get(name):
            self.tags[tag] = []
        self.tags[tag].append(name)
        
    def byName(self, name):
        """Return a Snippets object if it is defined."""
        return self.snippets[name] if self.snippets[name] else None
    
    def missingExamples(self):
        result = []
        for d in self.names:
            if not self.snippets[d].hasExample():
                result.append(d)
        return result
    
    def read(self):
        """Read in all snippets and their examples."""
        self.names = self.readDirectory(__main__.appInfo.defPath, ['.ily'])
        xmps = self.readDirectory(__main__.appInfo.xmpPath, ['.ly'])
        
        # read all snippets
        for d in self.names:
            self.snippets[d] = Snippet(self, d)
        # read all examples, ignore missing ones
        print "xmps:", xmps
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
