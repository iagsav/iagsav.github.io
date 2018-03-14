search_1 <- function(imTree, dec, decFG, Nx, Levx, fls, flsdec, flt, oper, imInd, fullGrp)
{  
  # поиск изображения в дереве + функции по искажению изображения
  library(data.tree)
  
  fullGrp <- 0
  procdir <- '/home/vgai/result/'
  
  N <- Nx
  iprn <- -1  
  ddec <- dec
  
  #ddec <- buildDec(dimg, N, flt, oper)
  
  # поиск изображения в базе
  rind <- c()
  for (j in 1:N)
  {
    if (j == 1)
    {
      myind <- 1:length(dec[[j]])
    }
    else {
      myind <- (iprn*4 - 3):(iprn*4)      
    }
    
    # определяем максимум на j-ом уровне по каким-то своим признакам
    tmp <- matrix(0, length(ddec[[j]]), 15)
    for (k in myind)
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
    
    iind <- imax[2]
    iprn <- imax[1]
    
    if (tmp[imax[1], imax[2]] < 0)
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

  if (is.null(imList))
  {
    return(list(1, 1))
  }
  
  # груба оценка точности поиска
  zres <- which(imList == imInd)
  
  if (length(zres) == 0)
  {
    err1 <- 1
    return(list(1, 1))
  } else {
    err1 <- 0
  }
  
  # сначала надо загрузить описания предварительно найденных файлов
  flsdec1 <- flsdec[imList]
  
  myLev <- Levx
  
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
  
  ta <- imList[imax]
  
  if (length(which(ta == imInd)) == 0)
  {
    return(list(1, err1))
  } else {
    return(list(0, err1))
  }   
  
  
}