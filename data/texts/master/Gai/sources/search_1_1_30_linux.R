rm(list = ls())
gc()

MyNorm = function(data)
{
  data <- data - min(data)
  data <- data / max(data)
  
}

# поиск изображения в дереве + функции по искажению изображения
library(data.tree)
library(jpeg)

source("buildDec.R")
source("buildUimg.R")
source("imshow.R")

#procdir <- 'd:/Temp/'
procdir <- '/home/vgai/result/'


#fls <- list.files(procdir, pattern = "\\.jpg$", ignore.case = TRUE)
#flsdec <- list.files(procdir, pattern = "\\FL.rdata$", ignore.case = TRUE)

snrs <- c(20, 10, 0)
starts <- 1
prval <- c(seq(0.01, 1, 0.01))

load('fls.rdata')
load('flsdec.rdata')
load('resTree.rdata')

library(foreach)
library(doMC)
registerDoMC(8)  #change the 2 to your number of CPU cores  

load('fo.rdata')
N <- 3
  
# индекс искомого изображения
# здесь должен начинаться цикл
thr <- 1

#system.time(ddd <- foreach(imInd = 1:length(fls), .packages = 'jpeg', .export = c('imTree', 'fls', 'flsdec')) %dopar% # %dopar%
system.time(ddd <- foreach(imInd = 1:length(fls), .packages = 'jpeg', .export = c('imTree', 'fls', 'flsdec')) %dopar% # %dopar%
{              
  print(imInd)
  err1 <- matrix(0, 1, 8)
  somesnr <- c()
  starts <- 1
  
  img <- readJPEG(paste(procdir, fls[[imInd]], sep = ""))

  if (length(dim(img)) == 3)
  {
    img <- img[,,1] + img[,,2] + img[,,3];
    img <- img / 3;
  }  
  
  # нормализация изображения по амплитуде к [0; 1]
  img <- MyNorm(img)
  
  # формирование искажённого изображения
  
  for (t in 1:length(snrs))
  {
    # формирование искажённого изображения
    
    # формирование искажённого изображени
    srcsnr <- snrs[t]
    index = starts:length(prval)
    
    for (ipr in index)
    {
      ns <- rnorm(prod(dim(img)), 0, prval[ipr])
      dimg <- img + ns
      dimg <- MyNorm(dimg)
      snr <- 10*log10( sum(img ^ 2) / sum(ns ^ 2) )
      if (((snr - srcsnr) < thr) && (snr > 0))
      {
        starts <- ipr
        somesnr <- c(somesnr, prval[ipr])
        break
      }
    }
    
    dimg <- img + rnorm(prod(dim(img)), 0, prval[ipr])
    dimg <- MyNorm(dimg)
    
    ddec <- buildDec(dimg, N, flt, oper)
    
    # поиск изображения в базе
    rind <- c()
    for (j in 1:N)
    {
      # определяем максимум на j-ом уровне по каким-то своим признакам
      tmp <- matrix(0, length(ddec[[j]]), 15)
      for (k in 1:length(ddec[[j]]))
      {
        tmp[k,] <- ddec[[j]][[k]][[1]][1:15]
      }
      
      imax <- which(abs(tmp) == max(abs(tmp)), arr.ind = TRUE)
      
      iind <- imax[2]
      if (tmp[imax] < 0)
      {
        iind <- iind + 15
      }
      # iind - адрес узла на i-ом уровне разложения
      
      rind <- c(rind, iind)
    }
    
    pref <- "imTree"
    for (j in 1:length(rind))
    {
      pref <- paste(pref, sprintf('$children[[%d]]', rind[j]) ,sep = "")  
    }
    commStr <- paste(pref, "$myData", sep = "")
    
    # предварительный список изображений
    imList <- eval(parse(text = commStr))
    
    # груба оценка точности поиска
    zres <- which(imList == imInd)
    if (length(zres) == 0)
    {
      err1[t + 4] <- err1[t + 4] + 1
    }
    
    # сначала надо загрузить описания предварительно найденных файлов
    flsdec1 <- flsdec[imList]
    
    myLev <- 3
    
    ldec <- list(0)
    for (i in 1:length(flsdec1))
    {
      # сюда нужен код
      load(paste(procdir, flsdec1[i], sep = ""))
      
      j <- myLev
      tmpSource <- matrix(0, length(dec[[j]]), 15)
      for (k in 1:length(dec[[j]]))
      {
        tmpSource[k,] <- dec[[j]][[k]][[1]][1:15]
      }
      
      ldec[[i]] <- tmpSource
    }
    
    j <- myLev
    tmpSource <- matrix(0, length(ddec[[j]]), 15)
    for (k in 1:length(ddec[[j]]))
    {
      tmpSource[k,] <- ddec[[j]][[k]][[1]][1:15]
    }
    
    # окончательный поиск изображения
    imDiff2 <- c()
    for (i in 1:length(imList))
    {
      # сюда нужен код
      imDiff2[i] <- sum(abs(sign(tmpSource) - sign(ldec[[i]])))
    }
    
    imax <- which(abs(imDiff2) == min(abs(imDiff2)), arr.ind = TRUE)
    
    if (imList[imax] != imInd)
    {
      err1[t] <- err1[t] + 1
    } 
  }
  save(err1, file = paste(procdir, fls[[imInd]], 'err30.rdata', sep = ""))
  save(somesnr, file = paste(procdir, fls[[imInd]], 'normnoise.rdata', sep = ""))
  return(err1)
})

