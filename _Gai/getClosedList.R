# функци генерации списка замкнутых групп

getClosedList = function()
{
  load('fullGrp.rdata')
  load('fo.rdata')
  
  #aaa = [10 12 14 15; 5 7 8 13; 2 3 6 11; 0 1 4 9];
  
  aaa <- matrix(0, 4, 4)
  aaa[1,] = c(10, 12, 14, 15)
  aaa[2,] = c( 5,  7,  8, 13)
  aaa[3,] = c( 2,  3,  6, 11)
  aaa[4,] = c( 0,  1,  4,  9)
  
  Grp <- getFullList(oper)
  Cls <- matrix(0, 105, 4)
  
  
  r <- matrix(0, 4, 3)
  r[1, ] = c( 1,  1,  1)
  r[2, ] = c(-1, -1,  1)
  r[3, ] = c(-1,  1, -1)
  r[4, ] = c( 1, -1, -1)  
  
  
  l1 <- 0
  l2 <- 0
  
  for (i in 1:dim(fullGrp)[1])
  {
    g <-  fullGrp[i,]
    
    for (j in 1:4)
    {
      res <- matrix(1, 4, 4)
      for (p in 1:3)
      {
        ff <- oper[[g[p]]]
        if (r[j, p] == -1)
        {
          ff <- !ff
          res <- res*ff
        }
        else
        {
          res <- res*ff
        }
      }
      if (res[4,1] != 1)
      {
        l2 <- l2 + 1
        Cls[l2,] = aaa[res == 1]
      }
      
    }
    
  }
  
  return(Cls)
}