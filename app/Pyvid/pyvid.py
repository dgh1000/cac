
import sys
from PyQt4.QtGui import *
from PyQt4.QtCore import *
from graphemes import readGrapheme, Grapheme
from parse import parseFile
from MainWindow import MainWindow


      
def main():
   sceneFilename = "/haskell/App/Pyvid/scene.txt"
   buf = open(sceneFilename, "r").read()
   entries = parseFile(buf)
   es = []
   for keyword, d in entries:
      if keyword == "wait":
         es.append("wait")
      else:
         es.append(readGrapheme(keyword, d))
   app = QApplication(sys.argv)
   w = MainWindow(es)
   w.show()
   app.exec_()

main()

