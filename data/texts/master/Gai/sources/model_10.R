
setwd('d:\\Dropbox\\repository\\Rassmem\\linux\\good\\new\\')
# модель на основе операторов
# бинарное дерево

# загрузили библиотеку
library(data.tree)
modeltype <- 10
procdir <- '/home/vgai/result/'

#procdir <- 'd:\\Temp\\'

#load('fls.rdata')
fls <- list.files(procdir, pattern = "\\FL.rdata$", ignore.case = TRUE)

N <- 5

C1 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
C2 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

# загрузка дерева с заданной структурой
load('imTree_bin.rdata')

#load('(142-3)AABT001012.jpgFL.rdata')

# цикл по файлам
for (i in 1:length(fls))
#for (i in 1:3)
{
  rind <- c()
  iprn <- -1  
  # загрузка данных
  load(paste(procdir, fls[[i]], sep = ""))
  #load('0_IGP7071.JPGFL.rdata')

  # цикл по уровнм
  pref <- 'imTree$'
  for (j in 1:N)
    #for (j in 1:3)
  {
    
    # выбор на первом и остальных уровнх отличаютс
    # на первом просто выбираем, на остальных - с учётом
    if (j == 1)
    {
      # вообще наверное логичнее всего индексы в цикле ниже устанавливать.
      myind <- 1:length(dec[[j]])
    } else {
      myind <- (iprn*4 - 3):(iprn*4)      
    }
    
    # определем максимум на j-ом уровне по каким-то своим признакам
    # 1. выбираетс область дл анализа....
    tmp <- matrix(0, length(dec[[j]]), 15)
    
#    for (k in 1:length(dec[[j]]))
    for (k in myind)
    {
      tmp[k,] <- dec[[j]][[k]][[1]][1:15]
    }
    
    imax <- which(abs(tmp) == max(abs(tmp)), arr.ind = TRUE)
    
    if (prod(dim(imax)) > 2)
    {
      print(fls[[i]]) 
      imax <- imax[1,]
      imax <- as.matrix(imax)
    }
    
    if (prod(dim(imax)) == 2)
    {
      # print(fls[[i]]) 
      imax <- as.matrix(imax)
    }
    
    # imax[1] -- номер области на i-ом уровне
    
    maxs <- tmp[imax[1], ] 
    
    print(imax[1])
    
    iprn <- imax[1]
    
    ### 2. формирование описани области
    
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
      pref <- paste(pref, sprintf('children[[%d]]$', 1), sep = "")
      rind <- c(rind, 1)      
    }
    else
    {
      pref <- paste(pref, sprintf('children[[%d]]$', 2), sep = "")
      rind <- c(rind, 2)      
    }
    
    commStr <- paste(pref, "myData", sep = "")
    commStr <- paste(commStr, " <- c(", commStr, ",", i, ")", sep = "")
    #print(commStr)
    # и выполнем команду
    eval(parse(text = commStr))
    
  }
  print(rind)
  
}

save(imTree, file = paste('model_', modeltype, '.rdata', sep = ''))
