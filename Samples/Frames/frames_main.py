#-------------------------------------------------------------------------------
# Name:        frames_main
# Purpose:
#
# Author:      Priyatham
#
# Created:     30-05-2022
# Copyright:   (c) Priyatham 2022
# Licence:     <your licence>
#-------------------------------------------------------------------------------

from frames import *

myForm = Form(Application)

myFrame1 = Frame1(myForm, lbl="Top")
myFrame1.SetProps(Parent=myForm, Align="Top")
myFrame2 = Frame1(myForm, lbl="Bottom")
myFrame2.SetProps(Parent=myForm, Align="Top")

myForm.show()
