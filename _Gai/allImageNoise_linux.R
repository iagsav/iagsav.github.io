rm(list = ls())
gc()

# вообщем получается так, что в некоторой степени приближённо
# можно для ОСШ 20, 10 и 0 дБ можно брать СКО нормального шума
# 0.03, 0.1 и 0.3 - не всегда так.

MyNorm = function(data)
{
  data <- data - min(data)
  data <- data / max(data)
}

procdir <- '/home/vgai/result/'

# функци дл оценки параметров изображений
# и дл их искажений

#procdir <- 'd:/Temp/'
#fls <- list.files(procdir, pattern = "\\.jpg$", ignore.case = TRUE)
load('fls.rdata')

snrs <- c(20, 10, 0)
starts <- 1

prval <- c(seq(0.01, 1, 0.01))

library(foreach)
library(doMC)
registerDoMC(14)  #change the 2 to your number of CPU cores  
library(jpeg)

imIndxs = seq(from = 1, to = length(fls), by = 100)

system.time(foreach(mz = 1:length(imIndxs), .packages = 'jpeg') %dopar% # %dopar%
{              
  imInd <- imIndxs[mz]
  starts <- 1
  somesnr <- c()
  print(imInd)
  img <- readJPEG(paste(procdir, fls[[imInd]], sep = ""))

  if (length(dim(img)) == 3)
  {
    img <- img[,,1] + img[,,2] + img[,,3];
    img <- img / 3;
  }  
  
  # нормализаци изображени по амплитуде к [0; 1]
  img <- MyNorm(img)
  
  # формирование искажённого изображени
  for (t in 1:length(snrs))
  {
    # формирование искажённого изображени
    srcsnr <- snrs[t]
    thr <- 0.5
    for (ipr in starts:length(prval))
    {
      ns <- rnorm(prod(dim(img)), 0, prval[ipr])
      dimg <- img + ns
      dimg <- MyNorm(dimg)
      snr <- 10*log10( sum(img ^ 2) / sum(ns ^ 2) )
      if (((snr - srcsnr) < thr) && (snr > 0))
      {
        print(paste(snr, starts, prval[ipr]))
        starts <- ipr
        somesnr <- c(somesnr, prval[ipr])
        break
      }
    }
  }

  save(somesnr, file = paste(procdir, fls[[imInd]], 'normnoise.rdata', sep = ""))
})

