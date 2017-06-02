import sys
from PyQt4.QtGui import *
from PyQt4.QtCore import *
from graphemes import *
from video.core.show import *

def main():
   app = QApplication(sys.argv)
   d = { "lineNumber": 1,
         "id": 3,
         "string": "like",
         "font": "Times New Roman",
         "size": 16,
         "bold": "true",
         "italic": "false",
         "show": "true",
         "x": 0,
         "y": 0 }
   t = Text.read(d)
   print(t.show().toString())

main()


