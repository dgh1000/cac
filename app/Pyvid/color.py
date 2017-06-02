"""
Validate color names and create a QColor
"""
from PyQt4.QtGui import *


def toColor(string):
   """
   """
   if (QColor.isValidColor(string)):
      return QColor(string)
   else:
      msg = "'%s' is not a valid color"
      raise Exception(msg % string)
