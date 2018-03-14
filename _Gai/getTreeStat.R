# получение статистики по распределению элементов в узлах

# дерево хранится в imTree

N <- 3
pref <- "imTree"
leafAmn <- 30 # количество листьев


# цикл по уровням дерева
for (i in 1:3) 
{
  if (i == 1)
  {
    chldArr1  <- matrix("", 1, leafAmn)
    chldArr1_ <- matrix("", 1, leafAmn)
    sm <- 0 # контрольная сумма
    for (j in 1:leafAmn)
    {
      chldArr1[j]  <- sprintf("length(imTree$children[[%d]]$", j)
      
      commStr <- paste(chldArr1[j], "myData)", sep = "")
      res  <- eval(parse(text = commStr))
      sm <- sm + res
      
      chldArr1_[j] <- sprintf("%d : ", j)
      
      print(sprintf(paste(chldArr1_[j], " %d ", sep = ""), res))
    }
    print(sm) # контрольная сумма
  }
  else
  {
    chldArr2  <- matrix("", 1, (leafAmn ^ i))
    chldArr2_ <- matrix("", 1, (leafAmn ^ i))
    k <- 1
    sm <- 0 # контрольная сумма
    
    for (j in 1:(leafAmn ^ (i - 1)))
    {
      for (m in 1:leafAmn)
      {
        chldArr2[k]  <- paste(chldArr1[j],  sprintf("children[[%d]]$", m), sep = "")
        
        commStr <- paste(chldArr2[k], "myData)", sep = "")
        #print(commStr)
        res  <- eval(parse(text = commStr))        
        sm <- sm + res
        
        chldArr2_[k] <- paste(chldArr1_[j], sprintf("%d : ", m), sep = "")
        
        if (res > 2)
        {
          print(sprintf(paste(chldArr2_[k], " %d ", sep = ""), res))
          
          commStr <- substr(commStr,8,nchar(commStr)-1)
          res  <- eval(parse(text = commStr))        
          print(res)
        }
        
        k <- k + 1
      }
    }
    print(sm) # контрольная сумма
  
    chldArr1  <- chldArr2
    chldArr1_ <- chldArr2_
  }
}
