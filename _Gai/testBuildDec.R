# функция для тестирвоания buildDec

library(bmp)
library(png)
library(jpeg)
library(caTools)
library(tiff)

graphics.off();
source("buildDec.r")
source("imread.r")
source("buildUimg.r")

load('fo.rdata')

buildDec('xx2.jpg', 1, flt, oper)

