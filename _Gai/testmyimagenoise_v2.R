rm(list = ls())
library(jpeg)
library(foreach)

source("myimagenoise.R")
source("buildDec.R")
source("buildUimg.R")
source("imshow.R")
source("getfullGroup.R")

model_type <- 15
rough <- 1 # rough <- 1 = 3 ???????, rough <- 0 = 5 ???????

source(paste('search_', model_type, '.r', sep = ''))
load(paste('model_', model_type, '.rdata', sep = ''))

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
imIndxs <- seq(from = 1, to = length(fls), by = 100)
b <- c(2202, 3176, 3177, 3178:4000, 10024, 16441:16450, 16528, 17980, 26196, 30810:30850)
imIndxs <- setdiff(imIndxs, b) 

myres <- foreach(mz = 1:length(imIndxs), .packages = 'jpeg') %dopar%
{
  imIndx <- imIndxs[mz]
  
  print(imIndx)
  img <- readJPEG(paste(procdir, fls[[imIndx]], sep = ""))
  if (length(dim(img)) == 3)
  {
    img <- img[,,1] + img[,,2] + img[,,3];
    img <- img / 3;
  }  
  
  result <- matrix(0, 2, dim(par)[1])
  for (i in 1:dim(par)[1])
  {
    load(paste(procdir, fls[[imIndx]], i, 'noised.rdata', sep = ""))
    
    if (rough == 1)
    {
      Nx <- 3
      Levx <- 3
    } else {
      # for N-ary tree
      if ((model_type == 7) || (model_type == 1) || (model_type == 4))
      {
        Nx <- 3
        Levx <- 3
      }
    }
    
    commStr <- paste('search_', model_type, '(imTree, dec, decFG, Nx, Levx, fls, flsdec, flt, oper, imIndx, fullGrp, clsGrp, decCG)', sep = '')
    res <- eval(parse(text = commStr))
    result[1, i] <- result[1, i] + res[[1]]
    result[2, i] <- result[2, i] + res[[2]]
    print(paste(imIndx, i, res[[1]]))
  }
  
  result <- result
  
}

save(myres, file = paste('resmod', model_type, '_rough', rough, '.rdata', sep = ''))
