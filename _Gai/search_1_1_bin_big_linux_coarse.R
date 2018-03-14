rm(list = ls())
gc()

load('imTree_bin_filled.rdata')
MyNorm = function(data)
{
  data <- data - min(data)
  data <- data / max(data)
}

# поиск на основе операторов
# бинарное дерево

# поиск изображени в дереве + функции по искажению изображени
library(data.tree)
library(jpeg)
library(EBImage)

source("buildDec.R")
source("buildUimg.R")
source("imshow.R")

#scaleind <- c(seq(0.3, 0.95, 0.05), seq(1.1, 10, 0.1))
scaleind1 <- c(seq(10, 100, 10))

load('fo.rdata')
N <- 5
C1 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
C2 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

library(foreach)

if (Sys.info()['sysname'] == 'Linux')
{
  procdir <- '/home/vgai/result/'
  library(doMC)
  registerDoMC(3)  #change the 2 to your number of CPU cores  
  load('fls.rdata')
  load('flsdec.rdata')
}
if (Sys.info()['sysname'] == 'Windows')
{
  procdir <- 'd:/temp/'
  fls <- list.files(procdir, pattern = "\\.jpg$", ignore.case = TRUE)
  flsdec <- list.files(procdir, pattern = "\\FL.rdata$", ignore.case = TRUE)
  
}

sind = seq(from = 1, to = 50000, by = 400)

imInd <- 1

myLev <- 5
N <- 5

ddd <- foreach(imInd = sind, .packages = 'jpeg', .export = c('imTree', 'fls', 'flsdec')) %dopar% # %dopar%
{  
  
  # индекс искомого изображени
  print(imInd)
  # начинаем отсюда
  err1 <- matrix(0, 1, 2*length(scaleind1))
  
  img <- readJPEG(paste(procdir, fls[[imInd]], sep = ""))
  
  if (length(dim(img)) == 3)
  {
    img <- img[,,1] + img[,,2] + img[,,3];
  }  
  
  # нормализаци изображени по амплитуде к [0; 1]
  img <- MyNorm(img)
  
  for (myscale in 1:length(scaleind1))
  {
    # формирование искажённого изображения

    ind1 <- seq(1, dim(img)[1] - scaleind1[myscale], scaleind1[myscale])
    ind2 <- seq(1, dim(img)[2] - scaleind1[myscale], scaleind1[myscale])
    
    dimg <- img
    for (t1 in ind1)
    {
      for (t2 in ind2)
      {
        dimg[t1:(t1 + scaleind1[myscale]), t2:(t2 + scaleind1[myscale])] = mean(img[t1:(t1 + scaleind1[myscale]), t2:(t2 + scaleind1[myscale])])
      }
    }

    dimg <- MyNorm(dimg)
    ddec <- buildDec(dimg, N, flt, oper)
    
    rind <- c()
    
    # цикл по уровнм
    for (j in 1:N)
    {
      # определем максимум на j-ом уровне по каким-то своим признакам
      tmp <- matrix(0, length(ddec[[j]]), 15)
      for (k in 1:length(ddec[[j]]))
      {
        tmp[k,] <- ddec[[j]][[k]][[1]][1:15]
      }
      
      imax <- which(abs(tmp) == max(abs(tmp)), arr.ind = TRUE)
      
      if (prod(dim(imax)) > 2)
      {
        #print(fls[[i]]) 
        imax <- imax[1,]
        imax <- as.matrix(imax)
      }
      
      if (prod(dim(imax)) == 2)
      {
        # print(fls[[i]]) 
        imax <- as.matrix(imax)
      }
      
      maxs <- tmp[imax[1], ]    
      
      binrep <- matrix(0, 1,30)
      
      for (jk in 1:length(maxs))
      {
        if (maxs[jk] < 0)
        {
          binrep[jk + 15] <- 1
        }
        else
        {
          binrep[jk] <- 1
        }
      }
      
      dist1 <- sum(C1 == binrep)
      dist2 <- sum(C2 == binrep)
      
      if (dist1 == dist2)
      {
        warning('dist1 == dist2')
      }
      
      if (dist1 > dist2)
      {
        rind <- c(rind, 1)      
      }
      else
      {
        rind <- c(rind, 2)      
      }
    }
    
    pref <- "imTree"
    for (j in 1:length(rind))
    {
      pref <- paste(pref, sprintf('$children[[%d]]', rind[j]) ,sep = "")  
    }
    commStr <- paste(pref, "$myData", sep = "")
    
    # предварительный список изображений
    imList <- eval(parse(text = commStr))
    
    zres <- which(imList == imInd)
    if (length(zres) == 0)
    {
      err1[myscale + length(scaleind1)] <- err1[myscale + length(scaleind1)] + 1
    }
    
    # сначала надо загрузить описани предварительно найденных файлов
    flsdec1 <- flsdec[imList]
    
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
    
    # окончательный поиск изображени
    imDiff2 <- c()
    for (i in 1:length(imList))
    {
      # сюда нужен код
      imDiff2[i] <- sum(abs(sign(tmpSource) - sign(ldec[[i]])))
    }
    
    imax <- which(abs(imDiff2) == min(abs(imDiff2)), arr.ind = TRUE)
    imax <- imax[1]
    #print(paste(imList[imax], imInd))
    if (imList[imax] != imInd)
    {
      err1[myscale] <- err1[myscale] + 1
    }
    
  }
  save(err1, file = paste(procdir, fls[[imInd]], 'errcoarsebin.rdata', sep = ""))
  
  #return(err1)
}
