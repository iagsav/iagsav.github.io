# значит так. теперь надо пройтись по каталогам и для каждого изображения сгенерировать
# многоуровневые описания отдельно для фильтров, полных и замкнутых групп.
# потом эти описания будут использоваться во всех моделях памяти

rm(list = ls())
gc()

load('fo.rdata')
load('fullGrp.rdata')
load('clsGrp.rdata')

source("buildDec.R")
#source("imread.R")
source("buildUimg.R")
source("getfullGroup.R")
source("getclosedGroup.R")

# каталог с файлами изображений
#procdir <- '/home/vgai/images/tmp/'
procdir <- '/home/vgai/result/'

# считываем список файлов
#fls <- list.files(procdir, pattern = "\\.jpg$", ignore.case = TRUE)
load('fls.rdata')

library(foreach)
library(doMC)
registerDoMC(14)  #change the 2 to your number of CPU cores  

N <- 5 # число уровней разложени, тогда размер сегмента будет 64.

#system.time(foreach(j = 1:length(fls), .packages = 'jpeg') %dopar%
system.time(foreach(j = 10:10, .packages = 'jpeg') %dopar%
{
  dec <- buildDec(readJPEG(paste(procdir, fls[[j]], sep = "")), N, flt, oper)
  save(dec, file = paste(procdir, fls[[j]], 'FL.rdata', sep = ""))
  
  decFG <- list(list(0))
  for (i in (1:(N + 1)))
  {
    decFG[[i]] <- list(0);
    for (k in 1:length(dec[[i]]))
    {
      decFG[[i]][[k]] <- getfullGroup(unlist(dec[[i]][[k]][[1]]), fullGrp, unlist(dec[[i]][[k]][[2]]), oper)
    }
  }
  save(decFG, file = paste(procdir, fls[[j]], 'FG.rdata', sep = ""))
  
  
  decCG <- list(list(0))
  for (i in (1:(N + 1)))
  {
    decCG[[i]] <- list(0);
    for (k in 1:length(dec[[i]]))
    {
      decCG[[i]][[k]] <- getclosedGroup(unlist(dec[[i]][[k]][[1]]), clsGrp, unlist(dec[[i]][[k]][[2]]), oper)
    }
  }
  save(decCG, file = paste(procdir, fls[[j]], 'CG.rdata', sep = ""))
})