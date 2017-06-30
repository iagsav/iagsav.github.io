rm(list = ls())
library(jpeg)
library(foreach)

source("myimagenoise.R")
source("buildDec.R")
source("buildUimg.R")
source("imshow.R")
source("getfullGroup.R")
source("getclosedGroup.R")

load('fls.rdata')
load('flsdec.rdata')
load('fo.rdata')
load('fullGrp.rdata')
load('clsGrp.rdata')

procdir <- '/home/vgai/result/'

type <- 1
noise <- 1:3
par1 <- expand.grid(type, noise)

type <- 2
scaleind1 <- 1:length(c(seq(10, 100, 20)))
par2 <- expand.grid(type, scaleind1)

type <- 3
scaleind1 <- 1:length(c(seq(0.3, 0.9, 0.3), seq(1.1, 2, 0.4), seq(0.3, 0.9, 0.3), seq(1.1, 2, 0.4)))
par3 <- expand.grid(type, scaleind1)

type <- 4
angleind <- 1:length(seq(from = 1, to = 9, by = 1))
par4 <- expand.grid(type, angleind)

type <- 5
spotsize <- 1:length(seq(from = 0.05, to = 0.4, by = 0.1))
par5 <- expand.grid(type, spotsize)
par <- rbind(par1, par2, par3, par4, par5)

###

library(doMC)
registerDoMC(14)  #change the 2 to your number of CPU cores  
imIndxs = seq(from = 1, to = length(fls), by = 100)

foreach(mz = 1:length(imIndxs), .packages = 'jpeg') %dopar%
{
  imIndx <- imIndxs[mz]
  print(imIndx)
  
  img <- readJPEG(paste(procdir, fls[[imIndx]], sep = ""))
  
  if (length(dim(img)) == 3)
  {
    img <- img[,,1] + img[,,2] + img[,,3];
    img <- img / 3;
  }  
  
  for (di in 1:dim(par)[1])
  {
    dimg <- myimagenoise(img, par[di, 1], par[di, 2], procdir, fls[[imIndx]])

    if (par[di, 1] == 3)
    {
      Nx   <- dimg[[2]]
      Levx <- dimg[[3]]
      dimg <- dimg[[1]]
    } else {
      Nx <- 5
      Levx <- 5
    }
    
    N <- Nx
    
    dec <- buildDec(dimg, N, flt, oper)
    
    decFG <- list(list(0))
    for (i in (1:(N + 1)))
    {
      decFG[[i]] <- list(0)
      for (k in 1:length(dec[[i]]))
      {
        decFG[[i]][[k]] <- getfullGroup(unlist(dec[[i]][[k]][[1]]), fullGrp, unlist(dec[[i]][[k]][[2]]), oper)
      }
    }
    
    decCG <- list(list(0))
    for (i in (1:(N + 1)))
    {
      decCG[[i]] <- list(0)
      for (k in 1:length(dec[[i]]))
      {
        decCG[[i]][[k]] <- getclosedGroup(unlist(dec[[i]][[k]][[1]]), clsGrp, unlist(dec[[i]][[k]][[2]]), oper)
      }
    }    
    
    save(dec, decFG, decCG, Nx, Levx, file = paste(procdir, fls[[imIndx]], di, 'noised.rdata', sep = ""))
    
    print(paste(imIndx, di, sep = ':'))
  }
}
