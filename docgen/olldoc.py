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
        self.labelSnippets = QtGui.QLabel("Defined Snippets:")
        self.labelCategories = QtGui.QLabel("Used Categories:")
        self.labelTags = QtGui.QLabel("Used Tags:")
        
        self.teSnippets = QtGui.QTextEdit()
        self.teCategories = QtGui.QTextEdit()
        self.teTags = QtGui.QTextEdit()
        
        self.pbReread = QtGui.QPushButton("Read again")
        self.pbExit = QtGui.QPushButton("Exit")
        
    def createConnects(self):
        self.pbReread.clicked.connect(self.readSnippets)
        self.pbExit.clicked.connect(self.close)

    def createLayout(self):
        centralWidget = QtGui.QWidget()
        centralLayout = cl = QtGui.QGridLayout()

        cl.addWidget(self.labelOverview, 0, 0, 1, 3)
        cl.addWidget(self.labelSnippets, 2, 0)
        cl.addWidget(self.labelCategories, 2, 1)
        cl.addWidget(self.labelTags, 2, 2)
        cl.addWidget(self.teSnippets, 3, 0)
        cl.addWidget(self.teCategories, 3, 1)
        cl.addWidget(self.teTags, 3, 2)
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
        self.temporaryDisplay()
    
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
