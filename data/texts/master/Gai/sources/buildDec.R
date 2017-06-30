round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10 ^ n
  z = z + 0.5
  z = trunc(z)
  z = z/10 ^ n
  z*posneg
}

MyNorm = function(data)
{
  data <- data - min(data)
  data <- data / max(data)
  
}

# мама Макс

# функция для построения многоуровневого разложения изображения
buildDec = function(fname, N, flt, oper)
{
  # N - число уровней разложения
  # fname - имя + путь к файлу с изображением
  #img = imread(fname);
  
  #load('fo.rdata')
  
  img <- fname
  fname <- 0
  
  # тут изображение надо в серое преобразовать. допустим, сложить всё.
  if (length(dim(img)) == 3)
  {
    img <- img[,,1] + img[,,2] + img[,,3];
    img <- img / 3;
  }  
  
  # нормализация изображения по амплитуде к [0; 1]
  img <- MyNorm(img)
  
  # img <- matrix(1,2048,2048)
  
  #imshow(img)
  idata = list(list(img));
  for (i in 1:N)
  {
    p <- 1;
    idata[[i + 1]] <- list(0);
    
    for (j in 1:length(idata[[i]]))
    {
      timg <- idata[[i]][[j]];
      sz <- dim(timg);
      # print(sz)
      h <- sz[2];
      
      w <- sz[1];
      
      sh <- c(1, round2((1/2)*h, 0), round2((1/2)*h, 0) + 1, h);
      sw <- c(1, round2((1/2)*w, 0), round2((1/2)*w, 0) + 1, w);
      
      for (k in seq(1,4,2))
      {
        for (l in seq(1,4,2))
        {
          img_ <- timg[sw[l]:sw[l + 1], sh[k]:sh[k + 1]];
          idata[[i + 1]][[p]] <- img_;
          #imshow(img_)
          p <- p + 1;
        }
      }
    }
  }
  
  dec <- list(list(0));
  
  for (i in 1:(N + 1))
  {
    dec[[i]] <- list(0);
    for (j in 1:length(idata[[i]]))
    {
      timg = idata[[i]][[j]];
      dec[[i]][[j]] <- buildUimg(timg, flt)
      
    }
    
  }
  return(dec);
}  

