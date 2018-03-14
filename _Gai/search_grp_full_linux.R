rm(list = ls())
gc()

MyNorm = function(data)
{
  data <- data - min(data)
  data <- data / max(data)
  
}

# поиск изображени в дереве + функции по искажению изображени
library(data.tree)
library(jpeg)

source("buildDec.R")
source("buildUimg.R")
source("imshow.R")
source("getfullGroup.R")

#procdir <- 'd:/Temp/'
procdir <- '/home/vgai/result/'


#fls <- list.files(procdir, pattern = "\\.jpg$", ignore.case = TRUE)
#flsdec <- list.files(procdir, pattern = "\\FL.rdata$", ignore.case = TRUE)

snrs <- c(20, 10, 0)
starts <- 1
prval <- c(seq(0.01, 1, 0.01))

load('fls.rdata')
load('flsdec.rdata')
load('full_tree_grp.rdata')

library(foreach)
library(doMC)
registerDoMC(14)  #change the 2 to your number of CPU cores  

load('fo.rdata')
load('fullGrp.rdata')

# индекс искомого изображени
# здесь должен начинатьс цикл
thr <- 1

myLev <- 5
N <- 5

# количество выбираемых максимальных по массе групп
nmax <- 7

x5 <- c(1, 1, 0, 0)
x6 <- c(0, 0, 1, 1)
C1 <- rep(x5, 35)
C2 <- rep(x6, 35)


#system.time(ddd <- foreach(imInd = 1:length(fls), .packages = 'jpeg', .export = c('imTree', 'fls', 'flsdec')) %dopar% # %dopar%


system.time(ddd <- foreach(imInd = 1:length(fls), .packages = 'jpeg', .export = c('imTree', 'fls', 'flsdec')) %dopar% # %dopar%
{              
  myfile <- paste(procdir, fls[[imInd]], 'normnoise.rdata', sep = '')
  if (file.exists(myfile))
  {
    load(myfile)
    print(imInd)
    err1 <- matrix(0, 1, 8)
    
    img <- readJPEG(paste(procdir, fls[[imInd]], sep = ""))
    
    if (length(dim(img)) == 3)
    {
      img <- img[,,1] + img[,,2] + img[,,3];
      img <- img / 3;
    }  
    
    # нормализаци изображени по амплитуде к [0; 1]
    img <- MyNorm(img)
    
    # формирование искажённого изображени
    
    for (t in 1:length(somesnr))
    {
      dimg <- img + rnorm(prod(dim(img)), 0, somesnr[t])
      dimg <- MyNorm(dimg)
      
      dec <- buildDec(dimg, N, flt, oper)
      ddec <- dec
      
      decFG <- list(list(0))
      for (i in (1:(N + 1)))
      {
        decFG[[i]] <- list(0);
        for (k in 1:length(dec[[i]]))
        {
          decFG[[i]][[k]] <- getfullGroup(unlist(dec[[i]][[k]][[1]]), fullGrp, unlist(dec[[i]][[k]][[2]]), oper)
        }
      }
      
      # поиск изображени в базе
      rind <- c()
      for (j in 2:N)
      {
        # определем максимум на j-ом уровне по каким-то своим признакам
        tmp <- matrix(0, length(decFG[[j]]), 140)
        for (k in 1:length(decFG[[j]]))
        {
          # decFG[[2]][[1]]
          # decFG[[j]][[k]][[1]] - mul
          # decFG[[j]][[k]][[2]] - sum
          if (length(decFG[[j]][[k]][[1]])/2 >= nmax)
          {
            srt <- sort(decFG[[j]][[k]][[1]][2,], index.return = TRUE)
            tmp[k, decFG[[j]][[k]][[1]][1, srt$ix[1:nmax]]] <- decFG[[j]][[k]][[1]][2, srt$ix[1:nmax]]
          }
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
          imax <- as.matrix(imax)
        }
        
        maxs <- tmp[imax[1], ]    
        
        binrep <- matrix(0, 1, 140)
        binrep[maxs > 0] = 1
        
        dist1 <- sum(C1 * binrep)
        dist2 <- sum(C2 * binrep)     
        
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
      
      # груба оценка точности поиска
      zres <- which(imList == imInd)
      if (length(zres) == 0)
      {
        err1[t + 4] <- err1[t + 4] + 1
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
        imDiff2[i] <- sum(abs(sign(tmpSource) - sign(ldec[[i]])))
      }
      
      imax <- which(abs(imDiff2) == min(abs(imDiff2)), arr.ind = TRUE)
      
      if (imList[imax] != imInd)
      {
        err1[t] <- err1[t] + 1
      } 
    }
    save(err1, file = paste(procdir, fls[[imInd]], 'errgrp.rdata', sep = ""))
  }
})
