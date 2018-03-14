# версия для Linux
setwd('~/scripts/Rassmem/')
source('buildDec.R')
source('buildUimg.R')
load('fo.rdata')
procdir <- '~/images/tmp/'
fls <- list.files(procdir, pattern="\\.jpg$", ignore.case=TRUE)
library(foreach)
library(doMC)
library(jpeg)
registerDoMC(5)
system.time(foreach(j = 1:100, .packages = 'jpeg') %dopar%
{
  dec <- buildDec(readJPEG(paste(procdir, fls[[j]], sep = "")), 5, flt, oper)
  save(dec, file = paste(procdir, fls[[j]], '.rdata', sep = ""))
})