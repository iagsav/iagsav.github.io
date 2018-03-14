MyNorm = function(data)
{
  data <- data - min(data)
  data <- data / max(data)
}

# поиск на основе операторов
# бинарное дерево

# поиск изображения в дереве + функции по искажению изображения
library(data.tree)
library(jpeg)

source("buildDec.r")
source("buildUimg.r")
source("imshow.r")

# закрыть все изображения
graphics.off()

# procdir <- 'd:/Temp/'
procdir <- '/home/vgai/result/'

fls <- list.files(procdir, pattern = "\\.jpg$", ignore.case = TRUE)
snrs <- c(20, 15, 5, 0)

starts <- 1
#prval <- c(seq(0.001, 3, 0.001))
prval <- c(seq(0.01, 1, 0.01))

load('fo.rdata')
N <- 5
C1 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
C2 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
err <- matrix(0, 1, 4)

library(foreach)
library(doParallel)
library(R.utils)

cl <- makeCluster(2)
registerDoParallel(cl)

res <- list(0)
system.time(ddd <- foreach(imInd = 1:10, .packages = 'jpeg', .export = 'imTree') %dopar% # %dopar%
#for (imInd in 1:length(fls))
{  
  # индекс искомого изображения
  #imInd <- 2
  
  # начинаем отсюда
  err1 <- matrix(0, 1, 4)
  
  img <- readJPEG(paste(procdir, fls[[imInd]], sep = ""))
  
  if (length(dim(img)) == 3)
  {
    img <- img[,,1] + img[,,2] + img[,,3];
    img <- img / 3;
  }  
  
  # нормализация изображения по амплитуде к [0; 1]
  img <- MyNorm(img)
  
  for (t in 1:length(snrs))
  {
    # формирование искажённого изображения
    
    #dimg <- img + runif(prod(dim(img)), 0, 1)
    
    srcsnr <- snrs[t]
    if (t <= 3)
    {
      index = starts:length(prval)
      thr <- 1
    }
    else
    {
      index = starts:length(prval)
      thr <- 1
    }
    
    somesnr <- c()
    for (ipr in index)
    {
      ns <- rnorm(prod(dim(img)), 0, prval[ipr])
      dimg <- img + ns
      dimg <- MyNorm(dimg)
      snr <- 10*log10( sum(img ^ 2) / sum(ns ^ 2) )
      if (((snr - srcsnr) < thr) && (snr > 0))
      {
        #print(prval[ipr])
        print(snr)
        starts <- ipr
        break
        
      }

    }
    
    dimg <- img + rnorm(prod(dim(img)), 0, prval[ipr])
    dimg <- MyNorm(dimg)
    
    #imshow(dimg)
    
    ddec <- buildDec(dimg, N, flt, oper)
    
    # загрузка дерева с заданной структурой
    # load('imTree_bin.rdata')
    
    rind <- c()
    
    # цикл по уровням
    for (j in 1:N)
    {
      # определяем максимум на j-ом уровне по каким-то своим признакам
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
        # pref <- paste(pref, sprintf('children[[%d]]$', 1), sep = "")
        rind <- c(rind, 1)      
      }
      else
      {
        # pref <- paste(pref, sprintf('children[[%d]]$', 2), sep = "")
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
    
    #print(rind)
    #print(imList)
    
    # сначала надо загрузить описания предварительно найденных файлов
    flsdec <- list.files(procdir, pattern = "\\FL.rdata$", ignore.case = TRUE)
    flsdec <- flsdec[imList]
    
    myLev <- 5
    
    ldec <- list(0)
    for (i in 1:length(flsdec))
    {
      # сюда нужен код
      load(paste(procdir, flsdec[i], sep = ""))
      
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
    #imDiff <- c()
    #imDiff1 <- c()
    imDiff2 <- c()
    #imDiff3 <- c()
    for (i in 1:length(imList))
    {
      # сюда нужен код
      #imDiff[i] <- sum(abs(tmpSource - ldec[[i]]))
      
      #imDiff1[i] <- sum(abs(ddec[[1]][[1]][[1]] - ldec[[i]][[1]][[1]][[1]]))
      
      imDiff2[i] <- sum(abs(sign(tmpSource) - sign(ldec[[i]])))
      
      #imDiff3[i] <- sum(abs(sign(ddec[[1]][[1]][[1]]) - sign(ldec[[i]][[1]][[1]][[1]])))
      
    }
    #print(imDiff)
    #print(imDiff1)
    #print(imDiff2)
    #print(imDiff3)
    
    imax <- which(abs(imDiff2) == min(abs(imDiff2)), arr.ind = TRUE)
    
    #print(paste(imList[imax], imInd))
    if (imList[imax] != imInd)
    {
      err[t] <- err[t] + 1
      err1[t] <- err1[t] + 1
    }
    
  }
  #print(err1)
  #res[[imInd]] <- err1
  return(err1)
  
})