#-------------------------------------------------------------------------------
# Name:        frames_init
# Purpose:
#
# Author:      Priyatham
#
# Created:     30-05-2022
# Copyright:   (c) Priyatham 2022
# Licence:     <your licence>
#-------------------------------------------------------------------------------

from frames_implementation import *

Application.Initialize()

myForm = Form(Application)

myFrame1 = Frame1(myForm, lbl="Top Aligned Frame")
myFrame1.SetProps(Parent=myForm, Align="Top")
myFrame2 = Frame1(myForm, lbl="Bottom Aligned Frame")
myFrame2.SetProps(Parent=myForm, Align="Top")

Application.MainForm = myForm

myForm.show()
Application.Run()
myForm.Destroy()