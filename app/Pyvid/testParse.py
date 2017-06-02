from parse import Parser, parseFile
from graphemes import *


buf = '''\
[text id:3 x:100 y:100 font:"Times New Roman" size:16 bold:false italic:false
    show:true string:foo]
'''

def main():
   b = Parser(buf)
   lines = parseFile(b)
   for keyword, fields in lines:
      if keyword == "text":
         t = Text.read(fields)
         print( t.show().toString())
      else:
         print("Unknown keyword '%s'" % keyword)
      

main()

