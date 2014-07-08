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
        
        # populate lists with (filtered) contents of some directories
        self.definitions = self.readDirectory(self.defPath, ['.ily'])
        self.examples = self.readDirectory(self.xmpPath, ['.ly'])
        
        # list all definitions without a matching example
        tmp = set(self.examples)
        self.missingExamples = [xmp for xmp in self.definitions if xmp not in tmp]
    
    def readDirectory(self, dir, exts = []):
        """Read in the given dir and return a list with
        all entries matching the given exts filter"""
        result = []
        for item in os.listdir(dir):
            (file, ext) = os.path.splitext(item) 
            if ext in exts:
                result.append(file)
        return result
    
class MainWindow(QtGui.QMainWindow):
    def __init__(self, *args):
        QtGui.QMainWindow.__init__(self, *args)
        self.setWindowTitle("openlilylib documentation generator")
        
        self.createWidgets()
        
        # display list of defined snippets and missing examples
        self.resultList.addItem("Defined snippets:")
        self.resultList.addItems(appInfo.definitions)
        if appInfo.missingExamples:
            self.resultList.addItem("\nSnippets without example:")
            self.resultList.addItems(appInfo.missingExamples)

        # create, read and parse snippets
        self.snippets = snippets.Snippets().snippets
        
        # TEMPORARY!
        # Add the content of the definitions to the listview
        self.resultList.addItem("")
        for sn in self.snippets:
            for line in self.snippets[sn].definition.filecontent:
                self.resultList.addItem(line.rstrip('\n'))

        self.setCentralWidget(self.centralWidget)
    
    def createWidgets(self):
        self.labelOverview = QtGui.QLabel("Elements in " + appInfo.defPath + ":")
        self.resultList = QtGui.QListWidget()

        self.centralWidget = QtGui.QWidget()
        centralLayout = QtGui.QVBoxLayout()
        centralLayout.addWidget(self.labelOverview)
        centralLayout.addWidget(self.resultList)
        self.centralWidget.setLayout(centralLayout)

def main(argv):
    global appInfo, mainWindow
    app = QtGui.QApplication(argv)
    appInfo = AppInfo(argv[0])
    mainWindow = MainWindow()
    mainWindow.show()
    sys.exit(app.exec_())

if __name__ == "__main__":
    main(sys.argv)
