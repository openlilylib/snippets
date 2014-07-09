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
        
        # create, read and parse snippets
        self.snippets = snippets.Snippets()
        self.snippets.read()
        
        # TEMPORARY
        self.temporaryFileDump()
        self.resultList.addItem("Processed snippets")
        self.resultList.addItem("See results in /doc/categories.txt")
        
    
    def createComponents(self):
        self.labelOverview = QtGui.QLabel("Elements in " + appInfo.defPath + ":")
        self.resultList = QtGui.QListWidget()

    def createLayout(self):
        centralWidget = QtGui.QWidget()
        centralLayout = QtGui.QVBoxLayout()
        centralLayout.addWidget(self.labelOverview)
        centralLayout.addWidget(self.resultList)
        centralWidget.setLayout(centralLayout)
        self.setCentralWidget(centralWidget)
    
    def temporaryFileDump(self):
        numsnippets = ' (' + str(len(self.snippets.snippets) - 
                                 len(self.snippets.missingExamples())) + ')' 
        sns = ['Snippets' + numsnippets, '========', '']
        for s in self.snippets.names:
            if self.snippets.byName(s).hasExample():
                sns.append('- ' + s)
        
        numcats = ' (' + str(len(self.snippets.categories)) + ')'
        cats = ['Categories' + numcats, '==========', '']
        for c in self.snippets.categories:
            cats.append(c + ' (' + str(len(self.snippets.categories[c])) + ')')
            for i in self.snippets.categories[c]:
                cats.append('- ' + i)
                
        numtags = ' (' + str(len(self.snippets.tagnames)) + ')'
        tags = ['Tags' + numtags,  '====', '']
        for t in self.snippets.tags:
            tags.append(t + ' (' + str(len(self.snippets.tags[t])) + ')')
            for i in self.snippets.tags[t]:
                tags.append('- ' + i)
            tags.append('')
        
        outfile = os.path.join(appInfo.docPath, 'categories.txt')
        f = open(outfile, 'w')
        try:
            f.write("openlilylib contents\nList used categories and tags.\n\n")
            f.write('\n'.join(sns) + '\n\n')
            f.write('\n'.join(cats) + '\n\n')
            f.write('\n'.join(tags) + '\n')
        finally:
            f.close()
        
def main(argv):
    global appInfo, mainWindow
    app = QtGui.QApplication(argv)
    appInfo = AppInfo(argv[0])
    mainWindow = MainWindow()
    mainWindow.show()
    sys.exit(app.exec_())

if __name__ == "__main__":
    main(sys.argv)
