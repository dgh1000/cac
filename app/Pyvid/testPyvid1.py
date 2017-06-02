import sys
from PyQt4.QtGui import *
from PyQt4.QtCore import *
from graphemes import readGrapheme, Grapheme
from parse import parseString
from MainWindow import MainWindow

sceneFile = '''\
[text id:0 string:"foo" font:"Times New Roman" size:24 bold:false
 italic:true color:black show:true x:350 y:100]

'''

def main():
   entries = parseString(sceneFile)
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
