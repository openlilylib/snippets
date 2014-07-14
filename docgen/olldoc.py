#!/usr/bin/env python
# -*- coding: utf-8

import sys, os
from PyQt4 import QtCore,  QtGui
import snippets
import metadata

class AppInfo(QtCore.QObject):
    """Stores global information about the application
    and the content of the openlilylib directories."""
    
    def __init__(self, path):
        # determine the root paths of the different operations
        # based on the argument representing the path of the application.
        self.root = os.path.abspath(os.path.join(os.path.dirname(path), ".."))
        self.scriptPath = os.path.join(self.root, "docgen")
        self.docPath = os.path.join(self.root, "doc")
        self.defPath = os.path.join(self.root, "library", "oll")
        self.xmpPath = os.path.join(self.root, "usage-examples")
    
class MainWindow(QtGui.QMainWindow):
    def __init__(self, *args):
        QtGui.QMainWindow.__init__(self, *args)
        self.setWindowTitle("openlilylib documentation generator")
        self.createComponents()
        self.createLayout()
        self.createConnects()
        
        self.snippets = None
        self.readSnippets()
        
        # TEMPORARY
        self.temporaryFileDump()
        
    def createComponents(self):
        self.labelOverview = QtGui.QLabel("Library directory: " + appInfo.defPath)

        # Browsing Tree View
        self.labelBrowse = QtGui.QLabel("Browse Snippets:")
        self.tvBrowse = QtGui.QTreeView()
        self.modelBrowse = QtGui.QStandardItemModel()
        self.modelBrowse.setHorizontalHeaderLabels(['Browse snippets'])
        self.tvBrowse.setModel(self.modelBrowse)
        self.tvBrowse.setUniformRowHeights(True)
        self.tvBrowse.header().hide()

        # Snippet Definition
        self.labelDefinition = QtGui.QLabel("Snippet Definition:")
        self.metadataWidget = metadata.MetadataWidget(self)
        self.teDefinition = QtGui.QTextEdit()
        self.teDefinition.setReadOnly(True)

        # Snippet's Usage Example
        self.labelExample = QtGui.QLabel("Usage Example:")
        self.teExample = QtGui.QTextEdit()
        self.teExample.setReadOnly(True)

        # Buttons
        self.pbReread = QtGui.QPushButton("Read again")
        self.pbExit = QtGui.QPushButton("Exit")
    
    def createConnects(self):
        self.pbReread.clicked.connect(self.readSnippets)
        self.pbExit.clicked.connect(self.close)
        
        self.tvBrowse.clicked.connect(self.snippetRowClicked)

    def createLayout(self):
        centralWidget = QtGui.QWidget()
        centralLayout = cl = QtGui.QGridLayout()
        snDefinitionLayout = sl = QtGui.QVBoxLayout()
        snMetadataLayout = sm = QtGui.QGridLayout()
        
        
        cl.addLayout(sl, 3, 1)
        #sl.addLayout(sm)
        sl.addWidget(self.metadataWidget)
        sl.addWidget(self.teDefinition)

        cl.addWidget(self.labelOverview, 0, 0, 1, 3)
        cl.addWidget(self.labelBrowse, 2, 0)
        cl.addWidget(self.tvBrowse, 3, 0)
        cl.addWidget(self.labelDefinition, 2, 1)
#        cl.addWidget(self.teDefinition, 3, 1)
        cl.addWidget(self.labelExample, 2, 2)
        cl.addWidget(self.teExample, 3, 2)
        cl.addWidget(self.pbReread, 5, 0)
        cl.addWidget(self.pbExit, 5, 2)
        centralWidget.setLayout(centralLayout)
        self.setCentralWidget(centralWidget)
    
    def displaySnippetMetadata(self, snippet):
        dfn = snippet.definition
        self.leIncludeName.setText(snippet.name)
        self.leSnippetTitle.setText(dfn.headerFields['snippet-title'])
        self.leShortDesc.setText(' '.join(dfn.headerFields['snippet-short-description']))
        
    def readSnippets(self):
        # create, read and parse snippets
        if not self.snippets:
            self.snippets = snippets.Snippets()
        self.snippets.read()
        #TEMPORARY
        self.displayTree()
    
    def displayTree(self):
        self.modelBrowse.clear()

        numsnippets = ' (' + str(len(self.snippets.snippets)) + ')'
        byName = QtGui.QStandardItem('By Name' + numsnippets)
        for sn in self.snippets.names:
            byName.appendRow(QtGui.QStandardItem(sn))
        self.modelBrowse.appendRow(byName)

        byCategory = QtGui.QStandardItem('By Category')
        for c in self.snippets.categories['names']:
            numsnippets = ' (' + str(len(self.snippets.categories[c])) + ')'
            cat = QtGui.QStandardItem(c + numsnippets)
            byCategory.appendRow(cat)
            for s in self.snippets.categories[c]:
                cat.appendRow(QtGui.QStandardItem(s))
        self.modelBrowse.appendRow(byCategory)
        
        byTag = QtGui.QStandardItem('By Tag')
        for t in self.snippets.tags['names']:
            numsnippets = ' (' + str(len(self.snippets.tags[t])) + ')'
            tag = QtGui.QStandardItem(t + numsnippets)
            byTag.appendRow(tag)
            for s in self.snippets.tags[t]:
                tag.appendRow(QtGui.QStandardItem(s))
        self.modelBrowse.appendRow(byTag)
        
        byAuthor = QtGui.QStandardItem('By Author')
        for a in self.snippets.authors['names']:
            numsnippets = ' (' + str(len(self.snippets.authors[a])) + ')'
            author = QtGui.QStandardItem(a + numsnippets)
            byAuthor.appendRow(author)
            for s in self.snippets.authors[a]:
                author.appendRow(QtGui.QStandardItem(s))
        self.modelBrowse.appendRow(byAuthor)

    def snippetRowClicked(self, index):
        name = unicode(self.modelBrowse.itemFromIndex(index).text())
        snippet = self.snippets.byName(name)
        if snippet is not None:
            self.metadataWidget.showSnippet(snippet)
            self.teDefinition.setText(''.join(snippet.definition.bodycontent))
            self.teExample.setText(''.join(snippet.example.filecontent))

    def temporaryFileDump(self):
        outfile = os.path.join(appInfo.docPath, 'categories.txt')
        f = open(outfile, 'w')
        try:
            f.write("openlilylib contents\nList used categories and tags.\n\n")
            f.write('\n'.join(self.snippets.displaySnippets()) + '\n\n')
            f.write('\n'.join(self.snippets.displayCategories()) + '\n\n')
            f.write('\n'.join(self.snippets.displayTags()) + '\n')
        finally:
            f.close()
        
def main(argv):
    global appInfo, mainWindow
    app = QtGui.QApplication(argv)
    appInfo = AppInfo(argv[0])
    mainWindow = MainWindow()
    mainWindow.showMaximized()
    sys.exit(app.exec_())

if __name__ == "__main__":
    main(sys.argv)
