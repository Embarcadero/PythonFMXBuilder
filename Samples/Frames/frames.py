#-------------------------------------------------------------------------------
# Name:        frames
# Purpose:
#
# Author:      Priyatham
#
# Created:     30-05-2022
# Copyright:   (c) Priyatham 2022
# Licence:     <your licence>
#-------------------------------------------------------------------------------

from delphifmx import *

class Frame1(Frame):
    def __init__(self, Owner, lbl=""):
        self.label = Label(self)
        self.label.SetProps(Parent=self, Text=lbl, Align="Center")



