"""
Read a list of fonts and strings and determine the metrics for each
string.

The input file has the format as follows. There is one line for each
string/font, formatted as follows:
<font name>,<font size>,<bold flag>,<italics flag>,<string>

  where
    <font name>: string with font name. can contain spaces
    <font size>: integer
    <bold flag>: 'true' or 'false'
    <italics flag>: 'true' or 'false'
    <string>: the string to check the metrics of
"""

from PyQt4.QtCore import *
from PyQt4.QtGui import *

def readStringsFile(fn):
   out = []
   for l in open(fn, "r").readlines():
      t = l.split(",")
      if len(t) == 1:
         raise Exception("maybe a blank line in the strings file? " \
                         "something without five comma-separated " \
                         "values anyway. ")
      if len(t) != 5:
         msg = "There is a line in the strings file that does not have "\
               "five comma-separated values."
         raise Exception(msg)
      name,sizeS,boldS,italicS,string = t
      try:
         size = int(sizeS)
      except ValueError:
         msg = "In strings file, the size is not an integer"
         raise Exception(msg)
      if boldS == "true":
         bold = True
      elif boldS == "false":
         bold = False
      else:
         msg = "In strings file, the bold flag is not 'true' or 'false'"
         raise Exception(msg)
      if italicS == "true":
         italic = True
      elif italicS == "false":
         italic = False
      else:
         msg = "In strings file, the italic flag is not 'true' or 'false'"
         raise Exception(msg)
      out.append((name, size, bold, italic, string))
   return out

def main():
   # configuration
   stringsFile = "c:/haskell/App/Pyvid/strings.txt"
   metricsFile = "c:/haskell/App/Pyvid/metrics.txt"

   app = QApplication([])
   strings = readStringsFile(stringsFile)
   out = open(metricsFile, "w")
   for s in strings:
      name = s[0]
      size = s[1]
      bold = s[2]
      italic = s[3]
      f = QFont(name, size, bold, italic)
      fm = QFontMetrics(f)
      r = fm.boundingRect(s[4])
      o = "top:{} left:{} bottom:{} right:{}\n".format(r.top(),
                                                       r.left(),
                                                       r.bottom(),
                                                       r.right())
      out.write(o)

main()

   
