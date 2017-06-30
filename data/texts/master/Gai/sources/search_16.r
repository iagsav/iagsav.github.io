#search_16 <- function(imTree, dimg, Nx, Levx, fls, flsdec, flt, oper, imInd, fullGrp)
search_16 <- function(imTree, dec, decFG, Nx, Levx, fls, flsdec, flt, oper, imInd, fullGrp)
{  
  procdir <- '/home/vgai/result/'

  # поиск на основе операторов
  # бинарное дерево
  
  # поиск изображени в дереве + функции по искажению изображени
  library(data.tree)
  
  C1 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  C2 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

  N <- Nx
  myLev <- Levx
  
  #ddec <- buildDec(dimg, N, flt, oper)
  ddec <- dec
  
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
  
  if (is.null(imList))
  {
    return(list(1, 1))
  }    
  
  zres <- which(imList == imInd)
  if (length(zres) == 0)
  {
    err1 <- 1
    return(list(1, 1))
  } else {
    err1 <- 0
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
  ta <- imList[imax]
  
  if (length(which(ta == imInd)) == 0)
  {
    return(list(1, err1))
  } else {
    return(list(0, err1))
  } 

  
}