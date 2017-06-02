"""
This module contains the classes of all graphical items to appear in a
video. Each class has a 'read' static method that constructs that class
from a dict of values. read() will throw an exception if the values present
in the dictionary are missing or an illegal format/value.

This module also contains helper routines _readInt(), _readBool(), and
_readStr().

"""
from abc import ABCMeta, abstractmethod
from video.core.show import *
from PyQt4.QtGui import *
from PyQt4.QtCore import *
from color import toColor

class Grapheme(metaclass=ABCMeta):

   @staticmethod
   @abstractmethod
   def read(dictIn):
      return

   @abstractmethod
   def draw(self, gscene):
      return

   @abstractmethod
   def getGraphicsItems(self):
      """
      Returns list of QGraphicsItem's that make up this Grapheme.
      """
      return

def _readInt(name, ln, dic):
   if name in dic:
      try:
         d = int(dic[name])
      except ValueError:
         msg = "In scene/grapheme file data defined at line %d, for the "\
               "key '%s' expected an int value but got '%s'"
         raise Exception(msg % (ln, name, dic[name]))
      return d
   msg = "In scene/grapheme file data defined at line %d, no key '%s'"
   raise Exception(msg % (ln, name))

def _readBool(name, ln, dic):
   if name in dic:
      s = dic[name]
      if s == "true":
         return True
      elif s == "false":
         return False
      else:
         msg = "In scene/grapheme file data defined at line %d, for the "\
               "key '%s' expected true or false but got '%s'"
         raise Exception(msg % (ln, name, dic[name]))
      return d
   msg = "In scene/grapheme file data defined at line %d, no key '%s'"
   raise Exception(msg % (ln, name))

def _readStr(name, ln, dic):
   if name in dic:
      return dic[name]
   msg = "In scene/grapheme file data defined at line %d, no key '%s'"
   raise Exception(msg % (ln, name))

class Text(Grapheme):
   def __init__(self, id_, string, font, color, pos, show):
      """
      = INPUT VARIABLES
      - id_    Int
      - string string
      - font   QFont
      - color  QColor
      - pos    QPointF
      - show   boolean
      """
      self.id_ = id_
      self.string = string
      self.font = font
      self.color = color
      self.pos = pos
      self.show = show
      # Variable for remembering graphics items that make up this Grapheme
      self.gitems = []

   @staticmethod
   def graphicsItems(self):
      return self.gitems

   @staticmethod
   def read(dictIn):
      """
      = INPUT VARIABLES
      
      - dictIn  dict with following keys
          'lineNumber' : <int> (line number in which this data was defined
                                in the source text file)
          'id' : <int>
          'string' : <str>
          'font' : <str>  (font name)
          'size' : <int>  (font size)
          'bold' : <str> (must be 'true' or 'false')
          'italic' : <str> (must be 'true' or 'false')
          'color' : <str> (must be a color name)
          'show' : <str> (must be 'true' or 'false')
          'x' : <int>
          'y' : <int>
      """
      if "lineNumber" in dictIn:
         ln = dictIn["lineNumber"]
      else:
         raise Exception("kfd432")
      id_ = _readInt("id", ln, dictIn)
      string = _readStr("string", ln, dictIn)
      font = _readStr("font", ln , dictIn)
      size = _readInt("size", ln, dictIn)
      bold = _readBool("bold", ln, dictIn)
      italic = _readBool("italic", ln, dictIn)
      colorString = _readStr("color", ln, dictIn)
      color = toColor(colorString)
      show = _readBool("show", ln, dictIn)
      x = _readInt("x", ln, dictIn)
      y = _readInt("y", ln, dictIn)
      b = QFont.Bold if bold else QFont.Normal
      f = QFont(font, size, b, italic)
      return Text(id_, string, f, color, QPointF(x, y), show)

   def draw(self, gscene):
      """
      Draw self on the QGraphicsScene.
      
      = INPUT VARIABLES
      - gscene          QGraphicsScene
      """
      ti = QGraphicsSimpleTextItem(self.string, None, gscene)
      ti.setFont(self.font)
      ti.setBrush(QBrush(self.color))
      self.gitems = [ti]
      

   def show(self):
      return ShowComposite("Text",
         [ ShowSingle("id:%d" % self.id_),
           ShowSingle("string:%s" % self.string),
           ShowSingle("font family:%s" % self.font.family()),
           ShowSingle("point size:%d" % self.font.pointSize()),
           ShowSingle("bold:%s" % self.font.bold()),
           ShowSingle("italic:%s" % self.font.italic()),
           ShowSingle("show:%s" % self.show),
           ShowSingle("pos:(%d,%d)" % (round(self.pos.x()),
                                       round(self.pos.y()))),
           ], True)

def readGrapheme(keyword, dictIn):
   """
   Given one item in a scene file, create the grapheme (one of the
   following classes: Text, [others to be determined]) and return it.

   = INPUT VARIABLES
   - keyword: keyword of entry in scene file, such as 'text'
   - dictIn: dict representing fields in the entry in the scene file

   = RETURN VALUE
   - one grapheme. Object of one of classes Text, [others to be
     determined]
   """
   if keyword == "text":
      return Text.read(dictIn)
   else:
      if "lineNumber" in dictIn:
         msg = "In line %d, the keyword '%s' is not known"
         raise Exception(msg % (dictIn["lineNumber"], keyword))
      else:
         msg = "Internal error: passed dictionary with no line number"
         raise Exception(msg)
