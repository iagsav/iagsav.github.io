getFullList = function(oper)
{
  # input - operatoes
  # result will be saved as list
  tmat <- matrix(1, 4, 4)
  res <- list(0)
  
  res2 <- matrix(0, 35, 3)
  x <- 1;
  for (i in 1:15)
  {
    for (j in (i + 1):15)
    {
      for (k in (j + 1):15)
      {
        if ((i <= 15) && (j <= 15) && (k <= 15))
        {
          r <- oper[[i]] + oper[[j]] + oper[[k]]
          sm <- sum(((r > 0) == tmat))
          if (sm == 16)
          {
            #res[[x]] <- list(c(i,j,k))
            res2[x,] <- c(i,j,k)
            x <- x + 1
            
          }
        }
      }
    }
  }
  return(res2)
}

fullGrp <- getFullList(oper)

# 
# require(profr)
# require(ggplot2)
# x = profr(getFullList(oper))
# ggplot(x)
# 
#   Rprof('x.out')
#   getFullList(oper)# this is the function to profile
#   Rprof(NULL)
#   summaryRprof('x.out')



# 