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
        self.labelSnippets = QtGui.QLabel("Defined Snippets:")
        self.labelCategories = QtGui.QLabel("Used Categories:")
        self.labelTags = QtGui.QLabel("Used Tags:")
        
        self.teSnippets = QtGui.QTextEdit()
        self.teCategories = QtGui.QTextEdit()
        self.teTags = QtGui.QTextEdit()
        
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
        
        self.tvNavigate.clicked.connect(self.rowClicked)

    def createLayout(self):
        centralWidget = QtGui.QWidget()
        centralLayout = cl = QtGui.QGridLayout()

        cl.addWidget(self.labelOverview, 0, 0, 1, 3)
        cl.addWidget(self.labelBrowse, 2, 0)
        cl.addWidget(self.tvNavigate, 3, 0)
        cl.addWidget(self.labelSnippets, 2, 1)
        cl.addWidget(self.labelCategories, 2, 2)
        cl.addWidget(self.labelTags, 2, 3)
        cl.addWidget(self.teSnippets, 3, 1)
        cl.addWidget(self.teCategories, 3, 2)
        cl.addWidget(self.teTags, 3, 3)
        cl.addWidget(self.pbReread, 5, 0)
        cl.addWidget(self.pbExit, 5, 3)
        centralWidget.setLayout(centralLayout)
        self.setCentralWidget(centralWidget)#
    
    def readSnippets(self):
        # create, read and parse snippets
        if not self.snippets:
            self.snippets = snippets.Snippets()
        self.snippets.read()
        #TEMPORARY
        self.temporaryDisplay()
        self.displayTree()
    
    def displayTree(self):
        self.modelNavigate.clear()

        byName = QtGui.QStandardItem('By Name')
        for sn in self.snippets.names:
            byName.appendRow(QtGui.QStandardItem(sn))
        self.modelNavigate.appendRow(byName)

        byCategory = QtGui.QStandardItem('By Category')
        for c in self.snippets.catnames:
            cat = QtGui.QStandardItem(c)
            byCategory.appendRow(cat)
            for s in self.snippets.categories[c]:
                cat.appendRow(QtGui.QStandardItem(s))
        self.modelNavigate.appendRow(byCategory)
        
        byTag = QtGui.QStandardItem('By Tag')
        for t in self.snippets.tagnames:
            tag = QtGui.QStandardItem(t)
            byTag.appendRow(tag)
            for s in self.snippets.tags[t]:
                tag.appendRow(QtGui.QStandardItem(s))
        self.modelNavigate.appendRow(byTag)
        
        byAuthor = QtGui.QStandardItem('By Author')
        for a in self.snippets.authornames:
            author = QtGui.QStandardItem(a)
            byAuthor.appendRow(author)
            for s in self.snippets.authors[a]:
                author.appendRow(QtGui.QStandardItem(s))
        self.modelNavigate.appendRow(byAuthor)

    def rowClicked(self, index):
        #TODO: continue to work here. Doesn't really make sense so far
        row = index.row()
        parent = index.parent()
        snippet = self.modelNavigate.data(self.modelNavigate.index(row, 0, parent), QtCore.Qt.UserRole)
        print "row clicked:", snippet
        
    def temporaryDisplay(self):
        self.teSnippets.setText('\n'.join(self.snippets.displaySnippets()))
        self.teCategories.setText('\n'.join(self.snippets.displayCategories()))
        self.teTags.setText('\n'.join(self.snippets.displayTags()))

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
