buildUimg = function(img, flt)
{
  sz <- dim(img);
  h <- sz[2];
  w <- sz[1];
  
  sh = c(1, round2((1/4) * h, 0), round2((1/4) * h, 0) + 1, round2((2/4) * h, 0), round2((2/4) * h, 0) + 1, round2((3/4) * h, 0), round2((3/4) * h, 0) + 1, h);
  sw = c(1, round2((1/4) * w, 0), round2((1/4) * w, 0) + 1, round2((2/4) * w, 0), round2((2/4) * w, 0) + 1, round2((3/4) * w, 0), round2((3/4) * w, 0) + 1, w);
  
  i1 <- 1;
  j1 <- 1;
  
  mass <- matrix(0, 4, 4);
  
  for (j in seq(1,8,2))
  {
    for (i in seq(1,8,2))
    {  
      dt <- img[sw[j]:sw[j + 1], sh[i]:sh[i + 1]]
      
      mass[i1,j1] = sum(dt);
      j1 = j1 + 1;
    }

    j1 <- 1;
    i1 <- i1 + 1;
  } 
  
  spectr = matrix(0, 1, 16);
  for (i in 1:16)
  {
    spectr[i] = sum(flt[[i]] * mass);
  }
  
  return(list(spectr, mass));

}