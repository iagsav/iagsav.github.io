MyNorm = function(data)
{
  data <- data - min(data)
  data <- data / max(data)
}

myimagenoise <- function(img, mtype, par1, procdir, fname)
{
  library(EBImage)
  
  # шум
  if (mtype == 1)
  {
    fname2 <- paste(procdir, fname, 'normnoise.rdata', sep = '')
    load(fname2)
   
    # somesnr загружает из файла    
    ns <- rnorm(prod(dim(img)), 0,  somesnr[par1])
    dimg <- img + ns

    dimg <- MyNorm(dimg)
    return(dimg)     
  }
  
  # пониж разреш
  if (mtype == 2)
  {
    myscale <- par1
    scaleind1 <- c(seq(10, 100, 20))
    ind1 <- seq(1, dim(img)[1] - scaleind1[myscale], scaleind1[myscale])
    ind2 <- seq(1, dim(img)[2] - scaleind1[myscale], scaleind1[myscale])
    
    dimg <- img
    for (t1 in ind1)
    {
      for (t2 in ind2)
      {
        dimg[t1:(t1 + scaleind1[myscale]), t2:(t2 + scaleind1[myscale])] = mean(img[t1:(t1 + scaleind1[myscale]), t2:(t2 + scaleind1[myscale])])
      }
    }
    
    dimg <- MyNorm(dimg)
    return(dimg)     
  }
  
  # масштабир
  if (mtype == 3)
  {
    myscale <- par1
    
    scaleind1 <- c(seq(0.3, 0.9, 0.3), seq(1.1, 2, 0.4), seq(0.3, 0.9, 0.3), seq(1.1, 2, 0.4))
    scaleind2 <- c(seq(0.3, 0.9, 0.3), seq(1.1, 2, 0.4), seq(1.1, 2, 0.4),   seq(0.3, 0.9, 0.3))
    
    w1 = ceiling(scaleind1[myscale]*dim(img))[1]
    h1 = ceiling(scaleind2[myscale]*dim(img))[2]
    
    dimg <- resize(img, w = w1, h = h1)
    
    N <- -1
    myLev <- -1
    
    if ((min(dim(dimg)) > 64) & ((min(dim(dimg)) <= 128)))
    {
      N <- 1
      myLev <- 1  
    }
    if ((min(dim(dimg)) > 128) & ((min(dim(dimg)) <= 256)))
    {
      N <- 2
      myLev <- 2
    } 
    if ((min(dim(dimg)) > 256) & ((min(dim(dimg)) <= 512)))
    {
      N <- 3
      myLev <- 3
    }  
    if ((min(dim(dimg)) > 512) & ((min(dim(dimg)) <= 1024)))
    {
      N <- 4
      myLev <- 4
    }  
    if (min(dim(dimg)) > 1024)
    {
      N <- 5
      myLev <- 5
    }    

    dimg <- MyNorm(dimg)
    return(list(dimg, N, myLev))
  }  

  # поворот
  if (mtype == 4)
  {
    angle <- par1
    angleind = seq(from = 1, to = 9, by = 1)
    dimg <- rotate(img, angleind[angle], bg.col = 0, output.dim = dim(img))
    dimg <- MyNorm(dimg)
    return(dimg)    
  }
  
  # перекрытие
  if (mtype == 5)
  {
    spotsize <- seq(from = 0.05, to = 0.4, by = 0.1)
    a <- 1
    b1 <- min(dim(img))
    b2 <- min(dim(img))
    
    aHei <- floor(b1 * spotsize[par1])
    aWid <- floor(b2 * spotsize[par1])

    rx <- floor(a + (b1 - a)*runif(1, 0, 1))
    ry <- floor(a + (b2 - a)*runif(1, 0, 1))
    
    if (rx >= (b1 - aWid))
    {
      rx <- b1 - aWid
    }
    
    if (ry >= (b2 - aHei))
    {
      ry <- b2 - aHei
    }
    
    if (rx <= aWid)
    {
      rx <- aWid + 1
    }
    
    if (ry <= aHei)
    {
      ry <- aHei + 1
    }
    
    #print(paste(rx, ry))
    
    img[(rx - aWid):(rx + aWid - 1), (ry - aHei):(ry + aHei - 1)] = 0

    dimg <- img

    dimg <- MyNorm(dimg)
    return(dimg)
  }  
}