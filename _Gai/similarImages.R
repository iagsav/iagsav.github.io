MyNorm = function(data)
{
  data <- data - min(data)
  data <- data / max(data)
}

# отображение дл каждой модели похожих изображений, находщихс в 
# одном узле дерева

#setwd("d:\\Dropbox\\repository\\Rassmem\\")
procdir <- '/home/vgai/result/'

library(data.tree)
library(jpeg)
library(EBImage)
library(foreach)
library(doMC)
registerDoMC(14)  #change the 2 to your number of CPU cores  

load('fls.rdata')
load('flsdec.rdata')

model_type <- 16
load(paste('model_', model_type, '.rdata', sep = ''))

level <- 5
# количество изображений
nImg <- 20

inrow <- 5 # число изображений в строке
hei   <- 200 # размер изображения после перемасштабирования
flag  <- 0
rind  <- 1

addr <- expand.grid(seq(1,2), seq(1,2), seq(1,2), seq(1,2), seq(1,2))

#imTree$children[[1]]$children[[1]]$children[[1]]$children[[1]]$children[[1]]$children[[1]]$myData

z <- 0
foreach(pp = 1:dim(addr)[1], .packages = c('jpeg', 'EBImage')) %dopar%
{  
  imList <- imTree$children[[addr[pp, 1]]]$children[[addr[pp, 2]]]$children[[addr[pp, 3]]]$children[[addr[pp, 4]]]$children[[addr[pp, 5]]]$myData
  #z <- z + length(imList)
  #print(imList[1:10])
  # теперь тут загружаем описани, а потом ищем наиболее похожие
  # и формируем файлик с похожими, причём первым - искомое
  
   flsdec1 <- flsdec[imList]
  
  ldec <- list(0)
  for (i in 1:length(flsdec1))
  {
    # сюда нужен код
    load(paste(procdir, flsdec1[i], sep = ""))
    
    j <- level
    tmpSource <- matrix(0, length(dec[[j]]), 15)
    for (k in 1:length(dec[[j]]))
    {
      tmpSource[k,] <- dec[[j]][[k]][[1]][1:15]
    }
    
    ldec[[i]] <- tmpSource
  }
  
   for (sind in 1:length(imList))
   {
    #sind <- 100
    
    # тут загружаем разложение сравниваемого изображени
    ddec <- ldec[[sind]]
    
    # окончательный поиск изображени
    imDiff2 <- c()
    for (i in 1:length(imList))
    {
      # сюда нужен код
      imDiff2[i] <- sum(abs(sign(ddec) - sign(ldec[[i]])))
    }
    
    srt <- sort(imDiff2, decreasing = FALSE, index.return = TRUE)
    # теперь надо найти наиболее похожие ...
    
    imList2 <- imList[srt$ix[1:nImg]]
    
    imres <- list(0)
    imres2 <- list(0)
    
    p <- 1
    for (i in 1:length(imList2))
    {
      ind <- imList2[[i]]
      im <- readJPEG(paste(procdir, fls[[ind]], sep = ""))
      if (length(dim(im)) == 3)
      {
        im <- im[,,1] + im[,,2] + im[,,3];
      }    
      im <- MyNorm(im)
      
      im <- resize(im, w = hei, h = hei)
      
      imres[[i]] <- im
      
      if (i %% inrow == 0)
      {
        imres2[[p]] <- do.call(cbind, imres)
        p <- p + 1
        imres <- list(c())
      }
    }
    
    res <- do.call(rbind, imres2)
    
    writeJPEG(res, target = paste('/home/vgai/scripts/Rassmem/model_res/model_type_', model_type, '_sind_', imList[sind], '_addr_', paste(addr[pp,], collapse = ''), '.jpg', sep = ''))
     
  }
  
}
