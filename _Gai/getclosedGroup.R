getclosedGroup = function(sdata, clsGrp, iMass, oper)
{
  qq <- c(8, 4, 2, 1)
  arr <- matrix(0, 1, 27)
  arr[13] <- 1
  arr[21] <- 2
  arr[25] <- 3
  arr[1]  <- 4
  arr[27] <- 5
  arr[3]  <- 6
  arr[7]  <- 7
  arr[15] <- 8
  
  alc <- 1
  
  r_grp <- matrix(0, 1, 105)
  r_mas <- matrix(0, 1, 105)
  
  sp <- sdata[1:15]
  for (i in 1:dim(clsGrp)[1])
  {
    grp <- clsGrp[i,]
    ddx <- sp[grp];
    
    if (ddx[1]*ddx[2]*ddx[3]*ddx[4] == 0)
      next
    
    cht <- sum((ddx < 0));
    if ((cht == 1) || (cht == 3))
    {
      kr <- list(0, 0, 0, 0)
      
      for (jk in 1:4)
      {
        if (sign(sp[clsGrp[i, jk]]) == -1)
        {
          kr[[jk]] <- oper[[clsGrp[i, jk]]] == 0;
        }
        else
        {
          kr[[jk]] <- oper[[clsGrp[i, jk]]];
        }
      }
      
      map <- kr[[2]] * kr[[3]] + kr[[1]] * kr[[4]] # сумма операторов с одним или трем инверсными в группе
      gmass <- sum(map * iMass)
      r_mas[alc] <- gmass
      
      mymap <- sign(sp[grp])
      number <- arr[sum(mymap * qq) + 14] # минимум = 0.
      r_grp[alc] <- (i - 1) * 8 + number;
      
      alc <- alc + 1
    }
  }    
  
  if (alc == 1)
  {
    return(list(0))
  }
  else
  {
    return(list(r_grp[,1:alc - 1], r_mas[,1:alc - 1]))
  }
}
