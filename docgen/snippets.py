#!/usr/bin/env python
# -*- coding: utf-8

import os
from PyQt4 import QtCore

import __main__

class SnippetFile(QtCore.QObject):
    """Snippet file (both definition and usage example.
    Has a filename and a filecontent field
    and an abstract parseFile() method."""
    def __init__(self, filename):
        super(SnippetFile, self)
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
    def __init__(self, filename):
        super(SnippetDefinition, self).__init__(filename)
    
    def parseFile(self):
        #TODO: parse the definition file
        pass
        
class SnippetExample(SnippetFile):
    """Usage example for a snippet"""
    def __init__(self, filename):
        super(SnippetExample, self).__init__(filename)

    def parseFile(self):
        #TODO: parse the example file
        pass

class Snippet(QtCore.QObject):
    """Object representing a single snippet.
    Contains a definition and an example object."""
    def __init__(self, name):
        super(Snippet, self)
        self.name = name
        defFilename = os.path.join(__main__.appInfo.defPath, name) + '.ily'
        xmpFilename = os.path.join(__main__.appInfo.xmpPath, name) + '.ly'
        self.definition = SnippetDefinition(defFilename)
        self.example = SnippetExample(xmpFilename)

class Snippets(QtCore.QObject):
    """Object holding a dictionary of snippets"""
    def __init__(self):
        self.snippetList = __main__.appInfo.definitions
        self.snippets = {}
        for name in self.snippetList:
            self.snippets[name] = Snippet(name)
