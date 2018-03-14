getfullGroup = function(sdata, fullGrp, iMass, oper)
{
  # эта функци работает только дл одной части изображени
  # sdata - отклики фильтров, fullGrp - список полных групп
  # iMass - массы частей изображени
  
  # вычисление полных групп
  # полна группа допускает два описани – на операции пересечени ? ? ? ? ? ? (операци умно-
  # жени, число инверсий – чётно) и операции объединени ? ? +? ? +? ? (операции сложени,
  # число инверсий – нечётно);
  
  # load('fullGrp.rdata')
  
  qq <- c(4,2,1)
  arrm = matrix(0, 1, 13)
  arrm[7]  = 1;
  arrm[3]  = 2;
  arrm[1]  = 3;
  arrm[13] = 4;
  
  arrs = matrix(0, 1, 13)
  arrs[1]  = 1;
  arrs[13] = 2;
  arrs[11] = 3;
  arrs[7] = 4;
  
  m_i <- 1;
  s_i <- 1; 

  f_mul <- matrix(0,2,140)
  f_sum <- matrix(0,2,140)
  
  sp <- sdata[1:15]
  
  for (i in 1:dim(fullGrp)[1])
  {
    grp <- fullGrp[i,]
    ddx <- sp[grp];
    
    if (ddx[1]*ddx[2]*ddx[3] == 0)
      next
    cht <- sum((ddx < 0));

    mymap <- sign(sp[grp]);
    
    if ((cht == 0) || (cht == 2))
    {
      # чётное число - умножение
      
      number <- arrm[sum(mymap*qq) + 6]# minimum = 0.
      f_mul[1,m_i] <- (i - 1)*4 + number
      # mx_grp(mx_i, :) = sign(sp(Grp(k, :)))'.*Grp(k, :); реальна группа (со знаками)
      
      # вот код, что ниже будет нужен дл вычислени масс групп
      # код будет написан сразу с учётом числа инверсий
      # т.е. код дл полных на сложении и умножении отличаетс

      if (mymap[1] == -1)
      {
        m1 <- !oper[[grp[1]]]
      }
      else
      {
        m1 <- oper[[grp[1]]]
      }
      
      if (mymap[2] == -1)
      {
        m2 <- !oper[[grp[2]]]
      }
      else
      {
        m2 <- oper[[grp[2]]]
      }
      
      if (mymap[3] == -1)
      {
        m3 <- !oper[[grp[3]]]
      }
      else
      {
        m3 <- oper[[grp[3]]]
      }
      
      m_ = m1 * m2 * m3
      f_mul[2,m_i] = sum(m_ * iMass)

      m_i = m_i + 1;
      
      # но тут ещё надо допилить код дл вычислени масс групп
    }
    else
    {
      # нечётное число - сложение
      
      number = arrs[sum(mymap*qq) + 8] # minimum = 0.
      f_sum[1, s_i] = (i - 1)*4 + number     
      # mn_grp(mn_i, :) = sign(sp(Grp(k, :)))'.*Grp(k, :); % реальна группа (со знаками)
      
      if (mymap[1] == -1)
      {
        m1 <- !oper[[grp[1]]]
      }
      else
      {
        m1 <- oper[[grp[1]]]
      }
      
      if (mymap[2] == -1)
      {
        m2 <- !oper[[grp[2]]]
      }
      else
      {
        m2 <- oper[[grp[2]]]
      }
      
      if (mymap[3] == -1)
      {
        m3 <- !oper[[grp[3]]]
      }
      else
      {
        m3 <- oper[[grp[3]]]
      }      
      
      m_ = (m1 + m2 + m3) > 0
      f_sum[2,s_i] = sum(m_ * iMass)
      
      s_i = s_i + 1;
      
    }
  }
  
  if (m_i == 1)  
  {
    return(list(0, f_sum[,1:s_i - 1]))
  }
  if (s_i == 1)  
  {
    return(list(f_mul[,1:m_i - 1], 0))
    
  }
  if ((m_i == 1) && (s_i == 1))
  {
    return(list(0,0))
  }
  
  return(list(f_mul[,1:m_i - 1], f_sum[,1:s_i - 1]))
}