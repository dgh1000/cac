
import sys
from PyQt4.QtGui import *
from PyQt4.QtCore import *
from graphemes import Grapheme

class MainWindow(QWidget):

   def __init__(self, entries, parent=None):
      super().__init__(parent)
      # I believe my experiment of the YouTube aspect ratio showed
      # 850 wide by 500 high filled the frame.
      self.boundaryRect = QRectF(QPointF(-425, -250), QPointF(425, 250))
      self.gscene = QGraphicsScene(self.boundaryRect, self)
      self.view = QGraphicsView(self.gscene, self)
      layoutH = QHBoxLayout(self)
      layoutH.addWidget(self.view)
      self.entries = entries
      self.nextIdx = 0

   def keyPressEvent(self, evt):
      self.step()

   def step(self):
      while self.nextIdx < len(self.entries):
         e = self.entries[self.nextIdx]
         if e == "wait":
            self.nextIdx += 1
            break
         else:
            assert isinstance(e, Grapheme)
            e.draw(self.gscene)
         self.nextIdx += 1

