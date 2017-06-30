# функция для тестирвоания buildDec для параллельности

library(bmp)
library(png)
library(jpeg)
library(caTools)
library(tiff)
library(pryr)
library(shiny)
library(devtools)

setwd('d:/Dropbox/repository/Rassmem/')

graphics.off();
source("buildDec.r")
source("imread.r")
source("buildUimg.r")

load('fo.rdata')

procdir <- 'd:/Temp/'
# считываем список файлов
fls <- list.files(procdir)
#setwd(procdir)

img <- list(0)

# for (i in 1:50)
# {
#   img[[i]] <- imread(fls[i])
# }


library(foreach)
library(doParallel)
library(R.utils)

cl <- makeCluster(3)
registerDoParallel(cl)

res <- list(0)

# лучше записывать всё в файлик сразу, так как память тратится сильно.
# и много объектов не получится обработать

  system.time(foreach(j = 1:1, .packages = 'jpeg') %do% # %dopar%
  {
    dec <- buildDec(readJPEG(paste(procdir, fls[[j]], sep = "")), 6, flt, oper)
    save(dec, file = paste(procdir, fls[[j]], '.rdata', sep = ""))
  })

# тут распараллеливание не помогает
system.time(foreach(j = 1:10) %do% # %dopar%
{
  load(file = paste(procdir, fls[[j]], '.rdata', sep = ""))
  res[[j]] <- dec
})

r1 <- res[[1]];

# тут распараллеливание не помогает
system.time(r2 <- foreach(j = 1:1098) %do% # %dopar%
{
  sum(res[[j]][[1]][[1]][[2]] - r1[[1]][[1]][[2]])
})

stopCluster(cl)


z <- 1
zz <- matrix(rexp(2000000, rate=.1), ncol=1000)
sum(zz)
save(z,file='2.rdata')
zz <- matrix(rexp(2000000, rate=.1), ncol=1000)
sum(zz)
save(z,file='1.rdata')

