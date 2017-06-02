from abc import ABCMeta, abstractmethod

class X(metaclass=ABCMeta):

   @abstractmethod
   def foo(self, x):
      return

   @staticmethod
   @abstractmethod
   def s(x):
      return

class Y(X):
   def __init__(self, x):
      self.x = x

class CChar:

   def __init__(self, c, eof=False):
      self.c = c
      self.eof = eof

   def __eq__(self, c):
      if self.eof:
         return False
      else:
         return c == self.c

def main():
   x = CChar("f", True)
   print(x == "f")
   y = Y(3)

main()
