import re


class EChar:

   def __init__(self, c=None, eof=False):
      self.c = c
      self.eof = eof

   def __eq__(self, c):
      if self.eof:
         return False
      else:
         return c == self.c
      
   def isAlphaNum(self):
      if self.eof:
         return False
      else:
         return re.match(r"[a-zA-Z0-9]", self.c)

   def isWhiteSpace(self):
      if self.eof:
         return False
      else:
         return self.c in " \n\r\t"

   def theChar(self):
      if self.eof:
         raise Exception("at eof in EChar.theChar()")
      else:
         return self.c

   def __str__(self):
      if self.eof:
         return "<eof>"
      else:
         return self.c


class Parser:
   def __init__(self, buf):
      self.buf = buf
      self.idx = 0

   def parseChar(self, c):
      if self._char() == c:
         self.advance()
      else:
         self.errorMessage("expected '%s'" % c)

   def parseManySpaces(self):
      while self._char().isWhiteSpace():
         self.advance()

   def advance(self, errmsg="internal error"):
      if self.idx < len(self.buf):
         self.idx += 1

   def _char(self):
      if self.idx >= len(self.buf):
         return EChar(eof=True)
      else:
         return EChar(self.buf[self.idx])

   def parseAlphaNumWord(self):
      out = ""
      while True:
         c = self._char()
         if c.isAlphaNum():
            out += c.theChar()
            self.advance()
         else:
            break
      if len(out) == 0:
         self.errorMessage("expecting alphanum word")
      return out

   def currentCharIs(self, c):
      return self._char() == c

   def errorMessage(self, msg):
      line, col = self.computeLineColumn()
      raise Exception("About line %d, column %d: %s" % (line, col, msg))

   def computeLineColumn(self):
      line = 1
      col = 1
      for i in range(self.idx):
         col += 1
         if self.buf[i] == '\n':
            line += 1
            col = 1
      return line, col
      

def _parseUpToSpaceOrBracket(buf):
   r = ""
   while True:
      c = buf._char()
      if c.isWhiteSpace() or c == "]":
         return r
      else:
         r += c.theChar()
         buf.advance()

def _parseUpToQuote(buf):
   r = ""
   while True:
      c = buf._char()
      if c == '"':
         return r
      else:
         r += c.theChar()
         buf.advance()

def _parseField(buf):
   w = buf.parseAlphaNumWord()
   buf.parseChar(":")
   if buf.currentCharIs('"'):
      buf.advance()
      d = _parseUpToQuote(buf)
      buf.parseChar('"')
      buf.parseManySpaces()
   else:
      d = _parseUpToSpaceOrBracket(buf)
      buf.parseManySpaces()
   return (w, d)

def parseManyFields(buf):
   d = {}
   while True:
      if buf.currentCharIs("]"):
         return d
      else:
         k, v = _parseField(buf)
         d[k] = v


def _parseLine(buf):
   lineNum, _ = buf.computeLineColumn()
   w = buf.parseAlphaNumWord()
   buf.parseManySpaces()
   dic = parseManyFields(buf)
   buf.parseChar(']')
   buf.parseManySpaces
   dic["lineNumber"] = lineNum
   return w, dic

def _parseManyLines(buf):
   out = []
   if buf.currentCharIs("["):
      buf.parseChar("[")
      buf.parseManySpaces()
      w, dic = _parseLine(buf)
      out.append((w,dic))
   return out

# def parseFile(buf):
   # """
   # = INPUT VARIABLES
   # - text buffer representing scene item data

   # = RETURN VALUE
   # - [<one item data entry>] where
   #     <one item data entry> is (<item keyword>, { <field>:<data>})
   #     item keywords are
   #       'text'
         
   # """
   #buf.parseManySpaces()
   #v = _parseManyLines(buf)
   #return v

def parseString(buf):
   """
   Args:
       buf (str): input string buffer to parse
   Returns list of pairs (<keyword>, <data>)  where <keyword> is type of
       grapheme and data is its configuration.
   """
   p = Parser(buf)
   p.parseManySpaces()
   v = _parseManyLines(p)
   return v



   

