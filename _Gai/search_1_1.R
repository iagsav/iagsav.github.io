MyNorm = function(data)
{
  data <- data - min(data)
  data <- data / max(data)
  
}

# поиск изображени в дереве + функции по искажению изображени
library(data.tree)
library(jpeg)

source("buildDec.R")
source("buildUimg.R")
# source("imshow.R")

#procdir <- '/home/vgai/images/tmp/'
procdir <- '/home/vgai/result/'

fls <- list.files(procdir, pattern = "\\.jpg$", ignore.case = TRUE)

# тут нужен код, который загружает нужные индексы, дерево в соответствии с этими индексами,
# меняет массив fls в зависимости от индексов

# индекс искомого изображени
imInd <- 30000

img <- readJPEG(paste(procdir, fls[[imInd]], sep = ""))

load('fo.rdata')
N <- 3

if (length(dim(img)) == 3)
{
  img <- img[,,1] + img[,,2] + img[,,3];
  img <- img / 3;
}  

# нормализаци изображени по амплитуде к [0; 1]
img <- MyNorm(img)

# формирование искажённого изображени

#img <- img + runif(prod(dim(img)), 0, 0.5)
dimg <- img
dimg <- img + rnorm(prod(dim(img)), 0, 1)

dimg <- MyNorm(dimg)
writeJPEG(dimg, 'd.jpg')

# imshow(dimg)

ddec <- buildDec(dimg, N, flt, oper)

# поиск изображени в базе
rind <- c()
for (j in 1:N)
{
  # определем максимум на j-ом уровне по каким-то своим признакам
  tmp <- matrix(0, length(ddec[[j]]), 15)
  for (k in 1:length(ddec[[j]]))
  {
    tmp[k,] <- ddec[[j]][[k]][[1]][1:15]
  }
  
  imax <- which(abs(tmp) == max(abs(tmp)), arr.ind = TRUE)
  
    if (prod(dim(imax)) > 2)
    {
      imax <- imax[1,]
      imax <- as.matrix(imax)
    }

    if (prod(dim(imax)) == 2)
    {
      imax <- as.matrix(imax)
    }

    iind <- imax[2]
    if (tmp[imax[1], imax[2]] < 0)
    {
      iind <- iind + 15
    }
    # iind - адрес узла на j-ом уровне разложени
  
  rind <- c(rind, iind)
}

pref <- "imTree"
for (j in 1:length(rind))
{
  pref <- paste(pref, sprintf('$children[[%d]]', rind[j]) ,sep = "")  
}
commStr <- paste(pref, "$myData", sep = "")

# предварительный список изображений
imList <- eval(parse(text = commStr))

# сначала надо загрузить описани предварительно найденных файлов
flsdec <- list.files(procdir, pattern = "\\FL.rdata$", ignore.case = TRUE)
flsdec <- flsdec[imList]

myLev <- 3

ldec <- list(0)
for (i in 1:length(flsdec))
{
  # сюда нужен код
  load(paste(procdir, flsdec[i], sep = ""))

  j <- myLev 
  tmpSource <- matrix(0, length(dec[[j]]), 15)
  for (k in 1:length(dec[[j]]))
  {
    tmpSource[k,] <- dec[[j]][[k]][[1]][1:15]
  }

  ldec[[i]] <- tmpSource
}

j <- myLev
tmpSource <- matrix(0, length(ddec[[j]]), 15)
for (k in 1:length(ddec[[j]]))
{
  tmpSource[k,] <- ddec[[j]][[k]][[1]][1:15]
}

# окончательный поиск изображени

imDiff <- c()
imDiff1 <- c()
imDiff2 <- c()
imDiff3 <- c()
for (i in 1:length(imList))
{
  # сюда нужен код
  imDiff[i] <- sum(abs(tmpSource - ldec[[i]]))
  
  imDiff1[i] <- sum(abs(ddec[[1]][[1]][[1]] - ldec[[i]][[1]][[1]][[1]]))

  imDiff2[i] <- sum(abs(sign(tmpSource) - sign(ldec[[i]])))
  
  imDiff3[i] <- sum(abs(sign(ddec[[1]][[1]][[1]]) - sign(ldec[[i]][[1]][[1]][[1]])))
}

# imax <- which(abs(imDiff) == min(abs(imDiff)), arr.ind = TRUE)
imax <- which(abs(imDiff2) == min(abs(imDiff2)), arr.ind = TRUE)


print(paste(imList[imax], imInd))
print(rind)

