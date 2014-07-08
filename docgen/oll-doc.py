#!/usr/bin/env python
# -*- coding: utf-8

import sys, os
from PyQt4 import QtCore,  QtGui

class AppInfo:
    def __init__(self, path):
        self.root = os.path.abspath(os.path.join(os.path.dirname(path), ".."))
        self.scriptPath = os.path.join(self.root, "docgen")
        self.docPath = os.path.join(self.root, "doc")
        self.defPath = os.path.join(self.root, "library", "oll")
        self.xmpPath = os.path.join(self.root, "usage-examples")
    
class MainWindow(QtGui.QMainWindow):
    def __init__(self, *args):
        QtGui.QMainWindow.__init__(self, *args)
        self.labelHelloWorld = QtGui.QLabel(appInfo.xmpPath)
        self.setWindowTitle("openlilylib documentation generator")
        
        centralWidget = QtGui.QWidget()
        centralLayout = QtGui.QVBoxLayout()
        centralLayout.addWidget(self.labelHelloWorld)
        
        centralWidget.setLayout(centralLayout)
        self.setCentralWidget(centralWidget)

def main(argv):
    global appInfo
    app = QtGui.QApplication(argv)
    appInfo = AppInfo(argv[0])
    mainwindow = MainWindow()
    mainwindow.show()
    sys.exit(app.exec_())

if __name__ == "__main__":
    main(sys.argv)
