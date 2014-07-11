#!/usr/bin/env python
# -*- coding: utf-8

import sys, os
from PyQt4 import QtCore,  QtGui
import snippets

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
        self.labelOverview = QtGui.QLabel("Elements in " + appInfo.defPath + ":")
        self.labelBrowse = QtGui.QLabel("Browse Snippets:")
        self.labelDefinition = QtGui.QLabel("Snippet Definition:")
        self.labelExample = QtGui.QLabel("Usage Example:")
        
        self.teDefinition = QtGui.QTextEdit()
        self.teDefinition.setReadOnly(True)
        self.teExample = QtGui.QTextEdit()
        self.teExample.setReadOnly(True)
        
        self.tvNavigate = QtGui.QTreeView()
        self.modelNavigate = QtGui.QStandardItemModel()
        self.modelNavigate.setHorizontalHeaderLabels(['Browse snippets'])
        self.tvNavigate.setModel(self.modelNavigate)
        self.tvNavigate.setUniformRowHeights(True)
        self.tvNavigate.header().hide()

        self.pbReread = QtGui.QPushButton("Read again")
        self.pbExit = QtGui.QPushButton("Exit")
        
    def createConnects(self):
        self.pbReread.clicked.connect(self.readSnippets)
        self.pbExit.clicked.connect(self.close)
        
        self.tvNavigate.clicked.connect(self.snippetRowClicked)

    def createLayout(self):
        centralWidget = QtGui.QWidget()
        centralLayout = cl = QtGui.QGridLayout()

        cl.addWidget(self.labelOverview, 0, 0, 1, 3)
        cl.addWidget(self.labelBrowse, 2, 0)
        cl.addWidget(self.tvNavigate, 3, 0)
        cl.addWidget(self.labelDefinition, 2, 1)
        cl.addWidget(self.teDefinition, 3, 1)
        cl.addWidget(self.labelExample, 2, 2)
        cl.addWidget(self.teExample, 3, 2)
        cl.addWidget(self.pbReread, 5, 0)
        cl.addWidget(self.pbExit, 5, 2)
        centralWidget.setLayout(centralLayout)
        self.setCentralWidget(centralWidget)#
    
    def readSnippets(self):
        # create, read and parse snippets
        if not self.snippets:
            self.snippets = snippets.Snippets()
        self.snippets.read()
        #TEMPORARY
        self.displayTree()
    
    def displayTree(self):
        self.modelNavigate.clear()

        numsnippets = ' (' + str(len(self.snippets.snippets)) + ')'
        byName = QtGui.QStandardItem('By Name' + numsnippets)
        for sn in self.snippets.names:
            byName.appendRow(QtGui.QStandardItem(sn))
        self.modelNavigate.appendRow(byName)

        byCategory = QtGui.QStandardItem('By Category')
        for c in self.snippets.catnames:
            numsnippets = ' (' + str(len(self.snippets.categories[c])) + ')'
            cat = QtGui.QStandardItem(c + numsnippets)
            byCategory.appendRow(cat)
            for s in self.snippets.categories[c]:
                cat.appendRow(QtGui.QStandardItem(s))
        self.modelNavigate.appendRow(byCategory)
        
        byTag = QtGui.QStandardItem('By Tag')
        for t in self.snippets.tagnames:
            numsnippets = ' (' + str(len(self.snippets.tags[t])) + ')'
            tag = QtGui.QStandardItem(t + numsnippets)
            byTag.appendRow(tag)
            for s in self.snippets.tags[t]:
                tag.appendRow(QtGui.QStandardItem(s))
        self.modelNavigate.appendRow(byTag)
        
        byAuthor = QtGui.QStandardItem('By Author')
        for a in self.snippets.authornames:
            numsnippets = ' (' + str(len(self.snippets.authors[a])) + ')'
            author = QtGui.QStandardItem(a + numsnippets)
            byAuthor.appendRow(author)
            for s in self.snippets.authors[a]:
                author.appendRow(QtGui.QStandardItem(s))
        self.modelNavigate.appendRow(byAuthor)

    def snippetRowClicked(self, index):
        name = unicode(self.modelNavigate.itemFromIndex(index).text())
        snippet = self.snippets.byName(name)
        if snippet is not None:
            self.teDefinition.setText(''.join(snippet.definition.filecontent))
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
