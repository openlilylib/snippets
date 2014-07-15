#!/usr/bin/env python
# -*- coding: utf-8

from PyQt4 import QtCore, QtGui
import snippets

class MetadataWidget(QtGui.QFrame):
    def __init__(self, owner):
        self.mainwindow = owner
        self.snippet = None
        self.snippetName = ''
        super(MetadataWidget, self).__init__(owner)
        self.setFrameShape(QtGui.QFrame.StyledPanel)
        self.hide()
        self.createComponents()
        self.createLayout()
        self.createConnects()
    
    def createComponents(self):
        # UI elements for snippet metadata
        # are maintained in dictionaries
        self.hfLabels = {}
        self.hfLineEdits = {}
        self.hfTextEdits = {}
        
        # dictionary with display information
        self.fieldTitles = {
            'status': ("Snippet status:",  
                        "Is the snippet finished?"), 
            'snippet-source': ("Snippet source:", 
                        "Reference to a discussion or other\n" +
                        "sources of inspiration."), 
            'tags': ("Tags:", "Tags to sort and browse by"), 
            'snippet-short-description': ("Short description:", 
                        "Subheading telling what the snippet does."), 
            'snippet-todo': ("TODOs:", "Comments about bugs and feature requests."), 
            'first-lilypond-version': ("First known LilyPond version:", 
                        "Oldest LilyPond version the snippet is known to run with."), 
            'last-lilypond-version': ("Last known LilyPond version:", 
                        "Newest LilyPond version the snippet is known to run with."),
            'snippet-description': ("Description:", 
                        "Detailed description of what the snippet does."), 
            'snippet-category': ("Category:", "Category to sort and browse snippet by."), 
            'snippet-author': ("Author(s):", "(List of) Author(s) of the snippet."), 
            'snippet-title': ("Title:", "Snippet title.")}
        
        # field list for sorted traversal
        self.stdFieldNames = [
            'snippet-title', 
            'snippet-short-description', 
            'snippet-description', 
            'snippet-author', 
            'snippet-source', 
            'snippet-category', 
            'tags', 
            'first-lilypond-version', 
            'last-lilypond-version', 
            'status', 
            'snippet-todo']
        
        # list containing names of non-standard fields contained in the file.
        self.custFieldNames = []
        
        # create labels, line and text edits
        for f in self.stdFieldNames:
            self.hfLabels[f] = QtGui.QLabel(self.fieldTitles[f][0])
            self.hfLabels[f].setToolTip(self.fieldTitles[f][1])
            self.hfLineEdits[f] = QtGui.QLineEdit()
            self.hfLineEdits[f].setReadOnly(True)
            self.hfTextEdits[f] = QtGui.QPlainTextEdit()
            self.hfTextEdits[f].setReadOnly(True)
            self.hfLabels[f].setToolTip(self.fieldTitles[f][1])
            self.hfTextEdits[f].setToolTip(self.fieldTitles[f][1])
            self.hfLineEdits[f].setToolTip(self.fieldTitles[f][1])
            

    def createLayout(self):
        """Line and text edits are shown alternatingly
        but initially added both to the layout."""
        self.layout = QtGui.QGridLayout()
        row = 0
        for f in self.stdFieldNames:
            self.layout.addWidget(self.hfLabels[f], row, 0)
            self.layout.addWidget(self.hfLineEdits[f], row, 1)
            self.layout.addWidget(self.hfTextEdits[f], row + 1, 0, 1, 2)
            row += 2
        self.setLayout(self.layout)
    
    def createConnects(self):
        pass
    
    def setDataWidget(self, fieldName, row):
        """Set the content of the data widget corresponding
        to the field name. Determine whether it is a single line
        or multiline entry and fill/show the right widget."""
        
        
        te = self.hfTextEdits[fieldName]
        le = self.hfLineEdits[fieldName]
        content = self.snippet.definition.headerFields[fieldName]
        # if it's _not_ a list it's a oneline string
        multiline = isinstance(content, list)
        
        if multiline:
            te.setPlainText('\n'.join(content))
            # calculate suitable height of the textedit
            # (this may change in the future when we're making it editable)
            rowHeight = te.fontMetrics().lineSpacing()
            lineCount = len(content)
            te.setFixedHeight(lineCount * (rowHeight + 2) + 12)
            le.hide()
            te.show()
        else:
            # fill line edit with field content or default text
            if content is not None:
                le.setText(content)
            else:
                le.setText('Undefined')
            te.hide()
            le.show()
            
        
    def showSnippet(self, snippet):
        """Populate the UI elements with a snippet's metadata."""
        if ((self.snippet is not None) and (self.snippet.name == snippet.name)):
            return
        else:
            self.snippet = snippet

        # First populate the standard fields
        row = 0
        for f in self.stdFieldNames:
            self.setDataWidget(f, row)
            row += 2
        
        # remove non-standard fields existing in the previously opened snippet
        for f in self.custFieldNames:
            self.layout.removeWidget(self.hfLabels[f])
            self.hfLabels[f].close()
            self.layout.removeWidget(self.hfTextEdits[f])
            self.hfTextEdits[f].close()
            self.layout.removeWidget(self.hfLineEdits[f])
            self.hfLineEdits[f].close()
        self.custFieldNames = []
        
        # add UI elements for any non-standard header fields
        for f in snippet.definition.headerFields:
            if not f in self.stdFieldNames:
                self.custFieldNames.append(f)
                self.hfLabels[f] = QtGui.QLabel(f + ':')
                self.hfLineEdits[f] = QtGui.QLineEdit()
                self.hfLineEdits[f].setReadOnly(True)
                self.hfTextEdits[f] = QtGui.QPlainTextEdit()
                self.hfTextEdits[f].setReadOnly(True)
        self.custFieldNames.sort()
        print self.custFieldNames
        for f in self.custFieldNames:
            self.layout.addWidget(self.hfLabels[f], row, 0)
            self.setDataWidget(f, row)
            self.layout.addWidget(self.hfLineEdits[f], row, 1)
            self.layout.addWidget(self.hfTextEdits[f], row + 1, 0, 1, 2)
            row += 2
            
        self.show()
