#!/usr/bin/env python
# -*- coding: utf-8

import sys
from PyQt4 import QtCore,  QtGui

class MainWindow(QtGui.QMainWindow):
    def __init__(self, *args):
        QtGui.QMainWindow.__init__(self, *args)
        self.labelHelloWorld = QtGui.QLabel("Hello World!")
        self.setWindowTitle("openlilylib documentation generator")
        
        centralWidget = QtGui.QWidget()
        centralLayout = QtGui.QVBoxLayout()
        centralLayout.addWidget(self.labelHelloWorld)
        
        centralWidget.setLayout(centralLayout)
        self.setCentralWidget(centralWidget)

def main(argv):
    app = QtGui.QApplication(argv)
    mainwindow = MainWindow()
    mainwindow.show()
    sys.exit(app.exec_())

if __name__ == "__main__":
    main(sys.argv)
