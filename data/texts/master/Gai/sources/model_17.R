# модель на основе полных групп

# загрузили библиотеку
library(data.tree)
modeltype <- 17

#procdir <- 'd:/Temp/'
procdir <- '/home/vgai/result/'
#fls <- list.files(procdir, pattern = "\\FG.rdata$", ignore.case = TRUE)
load('flsfullgrp.rdata')

N <- 5

x5 <- c(1, 1, 0, 0)
x6 <- c(0, 0, 1, 1)
C1 <- rep(x5, 35)
C2 <- rep(x6, 35)

# загрузка дерева с заданной структурой
load('imTree_bin.rdata')

# количество выбираемых максимальных по массе групп
nmax <- 7

for (i in 1:length(fls))
{
  rind <- c()
  
  # загрузка данных
  load(paste(procdir, fls[[i]], sep = ""))
  #load('0_IGP7071.JPGFL.rdata')
  
   # цикл по уровнм
   pref <- 'imTree$'
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
  print(i)
  
}

#save(imTree, file = 'full_tree_grp.rdata')
save(imTree, file = paste('model_', modeltype, '.rdata', sep = ''))
