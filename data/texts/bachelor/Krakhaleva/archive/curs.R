library('jpeg')
library('bmp')
library('magick')
load('fo.rdata')
#Путь к изображениям
location <- readLines("fF.txt")
location2 <- readLines("sF.txt")
#img <- readJPEG(location) 
#img2 <- readJPEG(location2) 
img <- readJPEG(location)
img2 <- readJPEG(location2)
imgOldF <- img
imgOldS <- img2
if (length(dim(img)) == 3)
{
  img <- img[,,1] + img[,,2] + img[,,3];
  img <- img / 3;
} 

if (length(dim(img2)) == 3)
{
  img2 <- img2[,,1] + img2[,,2] + img2[,,3];
  img2 <- img2 / 3;
} 

#gray <- (img[,,1] + img[,,2] + img[,,3]) / 3
#gray2 <- (img2[,,1] + img2[,,2] + img2[,,3]) / 3
#writeJPEG(gray, target = location)   
#writeJPEG(gray2, target = location2)

img <- img - min(img)
img <- img / max(img)

img2 <- img2 - min(img2)
img2 <- img2 / max(img2)

round2 = function(x, n) {
  posneg <- sign(x)
  z <- abs(x)*10 ^ n
  z <- z + 0.5
  z <- trunc(z)
  z <- z/10 ^ n
  return(z*posneg)
}
szF <- dim(img) #задали размер матрицы
hF <- szF[2]
wF <- szF[1]
imgFirstSizeH<-hF-hF%%40
imgFirstSizeW<-wF-wF%%40
#arraySearchSFH <- array(0)
#arraySearchSFW <- array(0)
#for(i in 3:1000){
 # if(imgFirstSizeH/i > 10 & imgFirstSizeW/i > 10){
#  arraySearchSFH[[i]] <- imgFirstSizeH%/%i
#  arraySearchSFW[[i]] <- imgFirstSizeW%/%i
#  }
#}

#разделили матрицу на 10на10 матриц
shF = c(1, round2((1/40) * imgFirstSizeH, 0), round2((1/40) * imgFirstSizeH, 0) + 1, round2((2/40) * imgFirstSizeH, 0), round2((2/40) * imgFirstSizeH, 0) + 1, round2((3/40) * imgFirstSizeH, 0), round2((3/40) * imgFirstSizeH, 0) + 1, round2((4/40) * imgFirstSizeH, 0), round2((4/40) * imgFirstSizeH, 0) + 1, round2((5/40) * imgFirstSizeH, 0), round2((5/40) * imgFirstSizeH, 0) + 1, round2((6/40) * imgFirstSizeH, 0), round2((6/40) * imgFirstSizeH, 0) + 1, round2((7/40) * imgFirstSizeH, 0), round2((7/40) * imgFirstSizeH, 0) + 1, round2((8/40) * imgFirstSizeH, 0), round2((8/40) * imgFirstSizeH, 0) + 1, round2((9/40) * imgFirstSizeH, 0), round2((9/40) * imgFirstSizeH, 0) + 1, round2((10/40) * imgFirstSizeH, 0), round2((10/40) * imgFirstSizeH, 0) + 1, round2((11/40) * imgFirstSizeH, 0), round2((11/40) * imgFirstSizeH, 0) + 1, round2((12/40) * imgFirstSizeH, 0), round2((12/40) * imgFirstSizeH, 0) + 1, round2((13/40) * imgFirstSizeH, 0), round2((13/40) * imgFirstSizeH, 0) + 1, round2((14/40) * imgFirstSizeH, 0), round2((14/40) * imgFirstSizeH, 0) + 1, round2((15/40) * imgFirstSizeH, 0), round2((15/40) * imgFirstSizeH, 0) + 1, round2((16/40) * imgFirstSizeH, 0), round2((16/40) * imgFirstSizeH, 0) + 1, round2((17/40) * imgFirstSizeH, 0), round2((17/40) * imgFirstSizeH, 0) + 1, round2((18/40) * imgFirstSizeH, 0), round2((18/40) * imgFirstSizeH, 0) + 1, round2((19/40) * imgFirstSizeH, 0), round2((19/40) * imgFirstSizeH, 0) + 1, round2((20/40) * imgFirstSizeH, 0), round2((20/40) * imgFirstSizeH, 0) + 1, round2((21/40) * imgFirstSizeH, 0), round2((21/40) * imgFirstSizeH, 0) + 1, round2((22/40) * imgFirstSizeH, 0), round2((22/40) * imgFirstSizeH, 0) + 1, round2((23/40) * imgFirstSizeH, 0), round2((23/40) * imgFirstSizeH, 0) + 1, round2((24/40) * imgFirstSizeH, 0), round2((24/40) * imgFirstSizeH, 0) + 1, round2((25/40) * imgFirstSizeH, 0), round2((25/40) * imgFirstSizeH, 0) + 1, round2((26/40) * imgFirstSizeH, 0), round2((26/40) * imgFirstSizeH, 0) + 1, round2((27/40) * imgFirstSizeH, 0), round2((27/40) * imgFirstSizeH, 0) + 1, round2((28/40) * imgFirstSizeH, 0), round2((28/40) * imgFirstSizeH, 0) + 1, round2((29/40) * imgFirstSizeH, 0), round2((29/40) * imgFirstSizeH, 0) + 1, round2((30/40) * imgFirstSizeH, 0), round2((30/40) * imgFirstSizeH, 0) + 1, round2((31/40) * imgFirstSizeH, 0), round2((31/40) * imgFirstSizeH, 0) + 1, round2((32/40) * imgFirstSizeH, 0), round2((32/40) * imgFirstSizeH, 0) + 1, round2((33/40) * imgFirstSizeH, 0), round2((33/40) * imgFirstSizeH, 0) + 1, round2((34/40) * imgFirstSizeH, 0), round2((34/40) * imgFirstSizeH, 0) + 1, round2((35/40) * imgFirstSizeH, 0), round2((35/40) * imgFirstSizeH, 0) + 1, round2((36/40) * imgFirstSizeH, 0), round2((36/40) * imgFirstSizeH, 0) + 1, round2((37/40) * imgFirstSizeH, 0), round2((37/40) * imgFirstSizeH, 0) + 1, round2((38/40) * imgFirstSizeH, 0), round2((38/40) * imgFirstSizeH, 0) + 1,round2((39/40) * imgFirstSizeH, 0), round2((39/40) * imgFirstSizeH, 0) + 1,imgFirstSizeH)
swF = c(1, round2((1/40) * imgFirstSizeW, 0), round2((1/40) * imgFirstSizeW, 0) + 1, round2((2/40) * imgFirstSizeW, 0), round2((2/40) * imgFirstSizeW, 0) + 1, round2((3/40) * imgFirstSizeW, 0), round2((3/40) * imgFirstSizeW, 0) + 1, round2((4/40) * imgFirstSizeW, 0), round2((4/40) * imgFirstSizeW, 0) + 1, round2((5/40) * imgFirstSizeW, 0), round2((5/40) * imgFirstSizeW, 0) + 1, round2((6/40) * imgFirstSizeW, 0), round2((6/40) * imgFirstSizeW, 0) + 1, round2((7/40) * imgFirstSizeW, 0), round2((7/40) * imgFirstSizeW, 0) + 1, round2((8/40) * imgFirstSizeW, 0), round2((8/40) * imgFirstSizeW, 0) + 1, round2((9/40) * imgFirstSizeW, 0), round2((9/40) * imgFirstSizeW, 0) + 1, round2((10/40) * imgFirstSizeW, 0), round2((10/40) * imgFirstSizeW, 0) + 1, round2((11/40) * imgFirstSizeW, 0), round2((11/40) * imgFirstSizeW, 0) + 1, round2((12/40) * imgFirstSizeW, 0), round2((12/40) * imgFirstSizeW, 0) + 1, round2((13/40) * imgFirstSizeW, 0), round2((13/40) * imgFirstSizeW, 0) + 1, round2((14/40) * imgFirstSizeW, 0), round2((14/40) * imgFirstSizeW, 0) + 1, round2((15/40) * imgFirstSizeW, 0), round2((15/40) * imgFirstSizeW, 0) + 1, round2((16/40) * imgFirstSizeW, 0), round2((16/40) * imgFirstSizeW, 0) + 1, round2((17/40) * imgFirstSizeW, 0), round2((17/40) * imgFirstSizeW, 0) + 1, round2((18/40) * imgFirstSizeW, 0), round2((18/40) * imgFirstSizeW, 0) + 1, round2((19/40) * imgFirstSizeW, 0), round2((19/40) * imgFirstSizeW, 0) + 1, round2((20/40) * imgFirstSizeW, 0), round2((20/40) * imgFirstSizeW, 0) + 1, round2((21/40) * imgFirstSizeW, 0), round2((21/40) * imgFirstSizeW, 0) + 1, round2((22/40) * imgFirstSizeW, 0), round2((22/40) * imgFirstSizeW, 0) + 1, round2((23/40) * imgFirstSizeW, 0), round2((23/40) * imgFirstSizeW, 0) + 1, round2((24/40) * imgFirstSizeW, 0), round2((24/40) * imgFirstSizeW, 0) + 1, round2((25/40) * imgFirstSizeW, 0), round2((25/40) * imgFirstSizeW, 0) + 1, round2((26/40) * imgFirstSizeW, 0), round2((26/40) * imgFirstSizeW, 0) + 1, round2((27/40) * imgFirstSizeW, 0), round2((27/40) * imgFirstSizeW, 0) + 1, round2((28/40) * imgFirstSizeW, 0), round2((28/40) * imgFirstSizeW, 0) + 1, round2((29/40) * imgFirstSizeW, 0), round2((29/40) * imgFirstSizeW, 0) + 1, round2((30/40) * imgFirstSizeW, 0), round2((30/40) * imgFirstSizeW, 0) + 1, round2((31/40) * imgFirstSizeW, 0), round2((31/40) * imgFirstSizeW, 0) + 1, round2((32/40) * imgFirstSizeW, 0), round2((32/40) * imgFirstSizeW, 0) + 1, round2((33/40) * imgFirstSizeW, 0), round2((33/40) * imgFirstSizeW, 0) + 1, round2((34/40) * imgFirstSizeW, 0), round2((34/40) * imgFirstSizeW, 0) + 1, round2((35/40) * imgFirstSizeW, 0), round2((35/40) * imgFirstSizeW, 0) + 1, round2((36/40) * imgFirstSizeW, 0), round2((36/40) * imgFirstSizeW, 0) + 1, round2((37/40) * imgFirstSizeW, 0), round2((37/40) * imgFirstSizeW, 0) + 1, round2((38/40) * imgFirstSizeW, 0), round2((38/40) * imgFirstSizeW, 0) + 1,round2((39/40) * imgFirstSizeW, 0), round2((39/40) * imgFirstSizeW, 0) + 1,imgFirstSizeW)
#shF = c(1, round2((1/20) * imgFirstSizeH, 0), round2((1/20) * imgFirstSizeH, 0) + 1, round2((2/20) * imgFirstSizeH, 0), round2((2/20) * imgFirstSizeH, 0) + 1, round2((3/20) * imgFirstSizeH, 0), round2((3/20) * imgFirstSizeH, 0) + 1, round2((4/20) * imgFirstSizeH, 0), round2((4/20) * imgFirstSizeH, 0) + 1, round2((5/20) * imgFirstSizeH, 0), round2((5/20) * imgFirstSizeH, 0) + 1, round2((6/20) * imgFirstSizeH, 0), round2((6/20) * imgFirstSizeH, 0) + 1, round2((7/20) * imgFirstSizeH, 0), round2((7/20) * imgFirstSizeH, 0) + 1, round2((8/20) * imgFirstSizeH, 0), round2((8/20) * imgFirstSizeH, 0) + 1, round2((9/20) * imgFirstSizeH, 0), round2((9/20) * imgFirstSizeH, 0) + 1, round2((10/20) * imgFirstSizeH, 0), round2((10/20) * imgFirstSizeH, 0) + 1, round2((11/20) * imgFirstSizeH, 0), round2((11/20) * imgFirstSizeH, 0) + 1, round2((12/20) * imgFirstSizeH, 0), round2((12/20) * imgFirstSizeH, 0) + 1, round2((13/20) * imgFirstSizeH, 0), round2((13/20) * imgFirstSizeH, 0) + 1, round2((14/20) * imgFirstSizeH, 0), round2((14/20) * imgFirstSizeH, 0) + 1, round2((15/20) * imgFirstSizeH, 0), round2((15/20) * imgFirstSizeH, 0) + 1, round2((16/20) * imgFirstSizeH, 0), round2((16/20) * imgFirstSizeH, 0) + 1, round2((17/20) * imgFirstSizeH, 0), round2((17/20) * imgFirstSizeH, 0) + 1, round2((18/20) * imgFirstSizeH, 0), round2((18/20) * imgFirstSizeH, 0) + 1, round2((19/20) * imgFirstSizeH, 0), round2((19/20) * imgFirstSizeH, 0) + 1, imgFirstSizeH)
#swF = c(1, round2((1/20) * imgFirstSizeW, 0), round2((1/20) * imgFirstSizeW, 0) + 1, round2((2/20) * imgFirstSizeW, 0), round2((2/20) * imgFirstSizeW, 0) + 1, round2((3/20) * imgFirstSizeW, 0), round2((3/20) * imgFirstSizeW, 0) + 1, round2((4/20) * imgFirstSizeW, 0), round2((4/20) * imgFirstSizeW, 0) + 1, round2((5/20) * imgFirstSizeW, 0), round2((5/20) * imgFirstSizeW, 0) + 1, round2((6/20) * imgFirstSizeW, 0), round2((6/20) * imgFirstSizeW, 0) + 1, round2((7/20) * imgFirstSizeW, 0), round2((7/20) * imgFirstSizeW, 0) + 1, round2((8/20) * imgFirstSizeW, 0), round2((8/20) * imgFirstSizeW, 0) + 1, round2((9/20) * imgFirstSizeW, 0), round2((9/20) * imgFirstSizeW, 0) + 1, round2((10/20) * imgFirstSizeW, 0), round2((10/20) * imgFirstSizeW, 0) + 1, round2((11/20) * imgFirstSizeW, 0), round2((11/20) * imgFirstSizeW, 0) + 1, round2((12/20) * imgFirstSizeW, 0), round2((12/20) * imgFirstSizeW, 0) + 1, round2((13/20) * imgFirstSizeW, 0), round2((13/20) * imgFirstSizeW, 0) + 1, round2((14/20) * imgFirstSizeW, 0), round2((14/20) * imgFirstSizeW, 0) + 1, round2((15/20) * imgFirstSizeW, 0), round2((15/20) * imgFirstSizeW, 0) + 1, round2((16/20) * imgFirstSizeW, 0), round2((16/20) * imgFirstSizeW, 0) + 1, round2((17/20) * imgFirstSizeW, 0), round2((17/20) * imgFirstSizeW, 0) + 1, round2((18/20) * imgFirstSizeW, 0), round2((18/20) * imgFirstSizeW, 0) + 1, round2((19/20) * imgFirstSizeW, 0), round2((19/20) * imgFirstSizeW, 0) + 1, imgFirstSizeW)


h1 <- shF[2]#Размеры матрицы
w1 <- swF[2]

kek <- 1
j1F <- 1
k1F <- 1
listOfMassF<-list(0)#Лист для подобластей
listMassF<-list(0)#Лист для масс
massF <- matrix(0, 4, 4)

for(i in 1:400)
{
  listOfMassF[[i]]<-list(matrix(0, nrow=swF[2],ncol=shF[2]))#разделение на лист матриц
}

for(i in 1:1600)
{
  listMassF[[i]]<-list(matrix(0, nrow=4,ncol=4))#лист для масс
}
#Получаем лист с матрицами от img
for (j in seq(1,80,2))
{
  for (i in seq(1,80,2))
  {  
    matr1 <- img[swF[i]:swF[i + 1], shF[j]:shF[j + 1]]
    listOfMassF[[j1F]] <- list(matr1)
    j1F = j1F + 1
    kek = kek + 1
  }
  kek = kek + 1
}

#Каждая матрица по-отдельности

for(i in 1:1600){
  
  j1F <- 1
  k1F <- 1
  mass <- matrix(0, 4, 4)  
  #размер каждой матрицы
  
  listProm<-matrix(unlist(listOfMassF[i]),nrow = w1, ncol = h1)
  
  shF1 = c(1, round2((1/4) * h1, 0), round2((1/4) * h1, 0) + 1, round2((2/4) * h1, 0), round2((2/4) * h1, 0) + 1, round2((3/4) * h1, 0), round2((3/4) * h1, 0) + 1, h1)
  swF1 = c(1, round2((1/4) * w1, 0), round2((1/4) * w1, 0) + 1, round2((2/4) * w1, 0), round2((2/4) * w1, 0) + 1, round2((3/4) * w1, 0), round2((3/4) * w1, 0) + 1, w1)
  
  for (j in seq(1,8,2))
  {
    for (k in seq(1,8,2))
    {  
      
      dt <- listProm[swF1[j]:swF1[j], shF1[k]:shF1[k]]
      mass[k1F,j1F] = sum(dt)
      listMassF[[i]] <- list(mass)#приведение матриц к размеру 4 на 4
      j1F = j1F + 1
    }
    
    j1F = 1
    k1F = k1F + 1
  }
}

spectrFirst <- list(0)
spectrFirstFin <-list(matrix(0,nrow=4,ncol=4))
for(j in 1:1600)
{ 
  listPredFirst<-matrix(unlist(listMassF[j]), nrow=4,ncol=4)
  for (i in 1:16)
  {
    spectrFirst[i]<-sum(flt[[i]] * listPredFirst)#Получение листа спектральных коэффицентов
  }
  spectrFirstFin[[j]]<-list(matrix(spectrFirst,nrow=4,ncol=4))
}

#Считаем срденквадратическое отклонение для кажой матрицы коэффицентов

listVal1 <- list(0)
listSD <- list(0)
for(i in 1:1600)
{ 
  for(j in 1:15)
  { 
    listVal1 <- unlist(spectrFirstFin[[i]])
  }
  listSD[[i]] <- sd(listVal1)
}
listSDMatr <- matrix(unlist(listSD),nrow = 40, ncol = 40)




#Работа со вторым изображением

szS <- dim(img2) #задали размер матрицы второго изображение
hS <- szS[2]
wS <- szS[1]
imgSecondSizeH<-hS-hS%%40
imgSecondSizeW<-wS-wS%%40
shS = c(1, round2((1/40) * imgSecondSizeH, 0), round2((1/40) * imgSecondSizeH, 0) + 1, round2((2/40) * imgSecondSizeH, 0), round2((2/40) * imgSecondSizeH, 0) + 1, round2((3/40) * imgSecondSizeH, 0), round2((3/40) * imgSecondSizeH, 0) + 1, round2((4/40) * imgSecondSizeH, 0), round2((4/40) * imgSecondSizeH, 0) + 1, round2((5/40) * imgSecondSizeH, 0), round2((5/40) * imgSecondSizeH, 0) + 1, round2((6/40) * imgSecondSizeH, 0), round2((6/40) * imgSecondSizeH, 0) + 1, round2((7/40) * imgSecondSizeH, 0), round2((7/40) * imgSecondSizeH, 0) + 1, round2((8/40) * imgSecondSizeH, 0), round2((8/40) * imgSecondSizeH, 0) + 1, round2((9/40) * imgSecondSizeH, 0), round2((9/40) * imgSecondSizeH, 0) + 1, round2((10/40) * imgSecondSizeH, 0), round2((10/40) * imgSecondSizeH, 0) + 1, round2((11/40) * imgSecondSizeH, 0), round2((11/40) * imgSecondSizeH, 0) + 1, round2((12/40) * imgSecondSizeH, 0), round2((12/40) * imgSecondSizeH, 0) + 1, round2((13/40) * imgSecondSizeH, 0), round2((13/40) * imgSecondSizeH, 0) + 1, round2((14/40) * imgSecondSizeH, 0), round2((14/40) * imgSecondSizeH, 0) + 1, round2((15/40) * imgSecondSizeH, 0), round2((15/40) * imgSecondSizeH, 0) + 1, round2((16/40) * imgSecondSizeH, 0), round2((16/40) * imgSecondSizeH, 0) + 1, round2((17/40) * imgSecondSizeH, 0), round2((17/40) * imgSecondSizeH, 0) + 1, round2((18/40) * imgSecondSizeH, 0), round2((18/40) * imgSecondSizeH, 0) + 1, round2((19/40) * imgSecondSizeH, 0), round2((19/40) * imgSecondSizeH, 0) + 1, round2((20/40) * imgSecondSizeH, 0), round2((20/40) * imgSecondSizeH, 0) + 1, round2((21/40) * imgSecondSizeH, 0), round2((21/40) * imgSecondSizeH, 0) + 1, round2((22/40) * imgSecondSizeH, 0), round2((22/40) * imgSecondSizeH, 0) + 1, round2((23/40) * imgSecondSizeH, 0), round2((23/40) * imgSecondSizeH, 0) + 1, round2((24/40) * imgSecondSizeH, 0), round2((24/40) * imgSecondSizeH, 0) + 1, round2((25/40) * imgSecondSizeH, 0), round2((25/40) * imgSecondSizeH, 0) + 1, round2((26/40) * imgSecondSizeH, 0), round2((26/40) * imgSecondSizeH, 0) + 1, round2((27/40) * imgSecondSizeH, 0), round2((27/40) * imgSecondSizeH, 0) + 1, round2((28/40) * imgSecondSizeH, 0), round2((28/40) * imgSecondSizeH, 0) + 1, round2((29/40) * imgSecondSizeH, 0), round2((29/40) * imgSecondSizeH, 0) + 1, round2((30/40) * imgSecondSizeH, 0), round2((30/40) * imgSecondSizeH, 0) + 1, round2((31/40) * imgSecondSizeH, 0), round2((31/40) * imgSecondSizeH, 0) + 1, round2((32/40) * imgSecondSizeH, 0), round2((32/40) * imgSecondSizeH, 0) + 1, round2((33/40) * imgSecondSizeH, 0), round2((33/40) * imgSecondSizeH, 0) + 1, round2((34/40) * imgSecondSizeH, 0), round2((34/40) * imgSecondSizeH, 0) + 1, round2((35/40) * imgSecondSizeH, 0), round2((35/40) * imgSecondSizeH, 0) + 1, round2((36/40) * imgSecondSizeH, 0), round2((36/40) * imgSecondSizeH, 0) + 1, round2((37/40) * imgSecondSizeH, 0), round2((37/40) * imgSecondSizeH, 0) + 1, round2((38/40) * imgSecondSizeH, 0), round2((38/40) * imgSecondSizeH, 0) + 1,round2((39/40) * imgSecondSizeH, 0), round2((39/40) * imgSecondSizeH, 0) + 1,imgSecondSizeH)
swS = c(1, round2((1/40) * imgSecondSizeW, 0), round2((1/40) * imgSecondSizeW, 0) + 1, round2((2/40) * imgSecondSizeW, 0), round2((2/40) * imgSecondSizeW, 0) + 1, round2((3/40) * imgSecondSizeW, 0), round2((3/40) * imgSecondSizeW, 0) + 1, round2((4/40) * imgSecondSizeW, 0), round2((4/40) * imgSecondSizeW, 0) + 1, round2((5/40) * imgSecondSizeW, 0), round2((5/40) * imgSecondSizeW, 0) + 1, round2((6/40) * imgSecondSizeW, 0), round2((6/40) * imgSecondSizeW, 0) + 1, round2((7/40) * imgSecondSizeW, 0), round2((7/40) * imgSecondSizeW, 0) + 1, round2((8/40) * imgSecondSizeW, 0), round2((8/40) * imgSecondSizeW, 0) + 1, round2((9/40) * imgSecondSizeW, 0), round2((9/40) * imgSecondSizeW, 0) + 1, round2((10/40) * imgSecondSizeW, 0), round2((10/40) * imgSecondSizeW, 0) + 1, round2((11/40) * imgSecondSizeW, 0), round2((11/40) * imgSecondSizeW, 0) + 1, round2((12/40) * imgSecondSizeW, 0), round2((12/40) * imgSecondSizeW, 0) + 1, round2((13/40) * imgSecondSizeW, 0), round2((13/40) * imgSecondSizeW, 0) + 1, round2((14/40) * imgSecondSizeW, 0), round2((14/40) * imgSecondSizeW, 0) + 1, round2((15/40) * imgSecondSizeW, 0), round2((15/40) * imgSecondSizeW, 0) + 1, round2((16/40) * imgSecondSizeW, 0), round2((16/40) * imgSecondSizeW, 0) + 1, round2((17/40) * imgSecondSizeW, 0), round2((17/40) * imgSecondSizeW, 0) + 1, round2((18/40) * imgSecondSizeW, 0), round2((18/40) * imgSecondSizeW, 0) + 1, round2((19/40) * imgSecondSizeW, 0), round2((19/40) * imgSecondSizeW, 0) + 1, round2((20/40) * imgSecondSizeW, 0), round2((20/40) * imgSecondSizeW, 0) + 1, round2((21/40) * imgSecondSizeW, 0), round2((21/40) * imgSecondSizeW, 0) + 1, round2((22/40) * imgSecondSizeW, 0), round2((22/40) * imgSecondSizeW, 0) + 1, round2((23/40) * imgSecondSizeW, 0), round2((23/40) * imgSecondSizeW, 0) + 1, round2((24/40) * imgSecondSizeW, 0), round2((24/40) * imgSecondSizeW, 0) + 1, round2((25/40) * imgSecondSizeW, 0), round2((25/40) * imgSecondSizeW, 0) + 1, round2((26/40) * imgSecondSizeW, 0), round2((26/40) * imgSecondSizeW, 0) + 1, round2((27/40) * imgSecondSizeW, 0), round2((27/40) * imgSecondSizeW, 0) + 1, round2((28/40) * imgSecondSizeW, 0), round2((28/40) * imgSecondSizeW, 0) + 1, round2((29/40) * imgSecondSizeW, 0), round2((29/40) * imgSecondSizeW, 0) + 1, round2((30/40) * imgSecondSizeW, 0), round2((30/40) * imgSecondSizeW, 0) + 1, round2((31/40) * imgSecondSizeW, 0), round2((31/40) * imgSecondSizeW, 0) + 1, round2((32/40) * imgSecondSizeW, 0), round2((32/40) * imgSecondSizeW, 0) + 1, round2((33/40) * imgSecondSizeW, 0), round2((33/40) * imgSecondSizeW, 0) + 1, round2((34/40) * imgSecondSizeW, 0), round2((34/40) * imgSecondSizeW, 0) + 1, round2((35/40) * imgSecondSizeW, 0), round2((35/40) * imgSecondSizeW, 0) + 1, round2((36/40) * imgSecondSizeW, 0), round2((36/40) * imgSecondSizeW, 0) + 1, round2((37/40) * imgSecondSizeW, 0), round2((37/40) * imgSecondSizeW, 0) + 1, round2((38/40) * imgSecondSizeW, 0), round2((38/40) * imgSecondSizeW, 0) + 1,round2((39/40) * imgSecondSizeW, 0), round2((39/40) * imgSecondSizeW, 0) + 1,imgSecondSizeW)
#shS = c(1, round2((1/20) * imgSecondSizeH, 0), round2((1/20) * imgSecondSizeH, 0) + 1, round2((2/20) * imgSecondSizeH, 0), round2((2/20) * imgSecondSizeH, 0) + 1, round2((3/20) * imgSecondSizeH, 0), round2((3/20) * imgSecondSizeH, 0) + 1, round2((4/20) * imgSecondSizeH, 0), round2((4/20) * imgSecondSizeH, 0) + 1, round2((5/20) * imgSecondSizeH, 0), round2((5/20) * imgSecondSizeH, 0) + 1, round2((6/20) * imgSecondSizeH, 0), round2((6/20) * imgSecondSizeH, 0) + 1, round2((7/20) * imgSecondSizeH, 0), round2((7/20) * imgSecondSizeH, 0) + 1, round2((8/20) * imgSecondSizeH, 0), round2((8/20) * imgSecondSizeH, 0) + 1, round2((9/20) * imgSecondSizeH, 0), round2((9/20) * imgSecondSizeH, 0) + 1, round2((10/20) * imgSecondSizeH, 0), round2((10/20) * imgSecondSizeH, 0) + 1, round2((11/20) * imgSecondSizeH, 0), round2((11/20) * imgSecondSizeH, 0) + 1, round2((12/20) * imgSecondSizeH, 0), round2((12/20) * imgSecondSizeH, 0) + 1, round2((13/20) * imgSecondSizeH, 0), round2((13/20) * imgSecondSizeH, 0) + 1, round2((14/20) * imgSecondSizeH, 0), round2((14/20) * imgSecondSizeH, 0) + 1, round2((15/20) * imgSecondSizeH, 0), round2((15/20) * imgSecondSizeH, 0) + 1, round2((16/20) * imgSecondSizeH, 0), round2((16/20) * imgSecondSizeH, 0) + 1, round2((17/20) * imgSecondSizeH, 0), round2((17/20) * imgSecondSizeH, 0) + 1, round2((18/20) * imgSecondSizeH, 0), round2((18/20) * imgSecondSizeH, 0) + 1, round2((19/20) * imgSecondSizeH, 0), round2((19/20) * imgSecondSizeH, 0) + 1, imgSecondSizeH)
#swS = c(1, round2((1/20) * imgSecondSizeW, 0), round2((1/20) * imgSecondSizeW, 0) + 1, round2((2/20) * imgSecondSizeW, 0), round2((2/20) * imgSecondSizeW, 0) + 1, round2((3/20) * imgSecondSizeW, 0), round2((3/20) * imgSecondSizeW, 0) + 1, round2((4/20) * imgSecondSizeW, 0), round2((4/20) * imgSecondSizeW, 0) + 1, round2((5/20) * imgSecondSizeW, 0), round2((5/20) * imgSecondSizeW, 0) + 1, round2((6/20) * imgSecondSizeW, 0), round2((6/20) * imgSecondSizeW, 0) + 1, round2((7/20) * imgSecondSizeW, 0), round2((7/20) * imgSecondSizeW, 0) + 1, round2((8/20) * imgSecondSizeW, 0), round2((8/20) * imgSecondSizeW, 0) + 1, round2((9/20) * imgSecondSizeW, 0), round2((9/20) * imgSecondSizeW, 0) + 1, round2((10/20) * imgSecondSizeW, 0), round2((10/20) * imgSecondSizeW, 0) + 1, round2((11/20) * imgSecondSizeW, 0), round2((11/20) * imgSecondSizeW, 0) + 1, round2((12/20) * imgSecondSizeW, 0), round2((12/20) * imgSecondSizeW, 0) + 1, round2((13/20) * imgSecondSizeW, 0), round2((13/20) * imgSecondSizeW, 0) + 1, round2((14/20) * imgSecondSizeW, 0), round2((14/20) * imgSecondSizeW, 0) + 1, round2((15/20) * imgSecondSizeW, 0), round2((15/20) * imgSecondSizeW, 0) + 1, round2((16/20) * imgSecondSizeW, 0), round2((16/20) * imgSecondSizeW, 0) + 1, round2((17/20) * imgSecondSizeW, 0), round2((17/20) * imgSecondSizeW, 0) + 1, round2((18/20) * imgSecondSizeW, 0), round2((18/20) * imgSecondSizeW, 0) + 1, round2((19/20) * imgSecondSizeW, 0), round2((19/20) * imgSecondSizeW, 0) + 1, imgSecondSizeW)


h2 <- shS[2]#Размеры матрицы
w2 <- swS[2]

j1S <- 1
k1S <- 1

listOfMassS<-list(0)
listMassS<-list(0)
massS <- matrix(0, 4, 4)

for(i in 1:1600)
{
  listOfMassS[[i]]<-list(matrix(0, nrow=swS[2],ncol=shS[2]))#разделение на лист матриц
}

for(i in 1:1600)
{
  listMassS[[i]]<-list(matrix(0, nrow=4,ncol=4))#лист для масс
}
#Получаем лист с матрицами от img
for (j in seq(1,80,2))
{
  for (i in seq(1,80,2))
  {  
    matr2 <- img2[swS[i]:swS[i + 1], shS[j]:shS[j + 1]]
    listOfMassS[[j1S]] <- list(matr2)
    j1S = j1S + 1
  }
}

#Каждая матрица по-отдельности 2

for(i in 1:1600){
  
  j1S <- 1
  k1S <- 1
  mass2 <- matrix(0, 4, 4)  
  #размер каждой матрицы
  
  listProm2<-matrix(unlist(listOfMassS[i]),nrow = w2, ncol = h2)
  
  shS1 = c(1, round2((1/4) * h2, 0), round2((1/4) * h2, 0) + 1, round2((2/4) * h2, 0), round2((2/4) * h2, 0) + 1, round2((3/4) * h2, 0), round2((3/4) * h2, 0) + 1, h2)
  swS1 = c(1, round2((1/4) * w2, 0), round2((1/4) * w2, 0) + 1, round2((2/4) * w2, 0), round2((2/4) * w2, 0) + 1, round2((3/4) * w2, 0), round2((3/4) * w2, 0) + 1, w2)
  
  for (j in seq(1,8,2))
  {
    for (k in seq(1,8,2))
    {  
      
      dt2 <- listProm2[swS1[j]:swS1[j], shS1[k]:shS1[k]]
      mass2[k1S,j1S] = sum(dt2)
      listMassS[[i]] <- list(mass2)#приведение матриц к размеру 4 на 4
      j1S = j1S + 1
    }
    
    j1S = 1
    k1S = k1S + 1
  }
}

spectrSecond <- list(0)
spectrSecondFin <-list(matrix(0,nrow=4,ncol=4))
for(j in 1:1600)
{ 
  listPredSecond<-matrix(unlist(listMassS[j]), nrow=4,ncol=4)
  for (i in 1:16)
  {
  spectrSecond[i]<-sum(flt[[i]] * listPredSecond)#Получение листа спектральных коэффицентов
  }
  spectrSecondFin[[j]]<-list(matrix(spectrSecond,nrow=4,ncol=4))
}


#Считаем срденквадратическое отклонение для кажой матрицы коэффицентов

listVal2 <- list(0)
listSD2 <- list(0)
for(i in 1:1600)
{ 
  for(j in 1:15)
  { 
    listVal2 <- unlist(spectrSecondFin[[i]])
  }
  listSD2[[i]] <- sd(listVal2)
}
listSD2Matr <- matrix(unlist(listSD2),nrow = 40, ncol = 40)

#Ищем максимальные значения и их позиции первого изображения
maxFirst<-array(0, 100)
maxFirstPos<-array(0, 100)
maxFirst[[1]]<-listSDMatr[[1]]
for(i in 1:1600){
  if(listSDMatr[[i]] > maxFirst[[1]]){
    maxFirst[[1]] <- listSDMatr[[i]]
    maxFirstPos[[1]] <- i
  }
}

for(i in 2:100){
  for(j in 1:1600){
    if(listSDMatr[[j]] < maxFirst[[i - 1]] & listSDMatr[[j]] > maxFirst[[i]]){
      maxFirst[[i]] <- listSDMatr[[j]]
      maxFirstPos[[i]] <- j
    }
  }
}

#Ищем максимальные значения и их позиции второго изображения
maxSecond<-array(0, 100)
maxSecondPos<-array(0, 100)
maxSecond[[1]]<-listSD2Matr[[1]]
for(i in 1:1600){
  if(listSD2Matr[[i]] > maxSecond[[1]]){
    maxSecond[[1]] <- listSD2Matr[[i]]
    maxSecondPos[[1]] <- i
  }
}

for(i in 2:100){
  for(j in 1:1600){
    if(listSD2Matr[[j]] < maxSecond[[i - 1]] & listSD2Matr[[j]] > maxSecond[[i]]){
      maxSecond[[i]] <- listSD2Matr[[j]]
      maxSecondPos[[i]] <- j
    }
  }
}

#Сравнение
a <- 1
compare <- array(0)
posCompareFirst <- list(0)
posCompareSecond <- list(0)
posCompareFirstV <- list(0)
posCompareSecondV <- list(0)
posPairs <- matrix(0, nrow = 2, ncol = 20)
posPairsVal <- matrix(0, nrow = 2, ncol = 20)
for(i in 1:100){
  for(j in 1:100){
    if((abs(maxFirst[[i]] - maxSecond[[j]]) < 0.1) & (maxFirstPos[[i]]%%20) == (maxSecondPos[[j]]%%20) & (abs(listSDMatr[[maxFirstPos[[i]] - 1]] - listSD2Matr[[maxSecondPos[[j]] - 1]]) < 0.1) & (abs(listSDMatr[[maxFirstPos[[i]] + 1]] - listSD2Matr[[maxSecondPos[[j]] + 1]]) < 0.1) & (abs(listSDMatr[[maxFirstPos[[i]] - 2]] - listSD2Matr[[maxSecondPos[[j]] - 2]]) < 0.1) & (abs(listSDMatr[[maxFirstPos[[i]] + 2]] - listSD2Matr[[maxSecondPos[[j]] + 2]]) < 0.1) & (abs(listSDMatr[[maxFirstPos[[i]] - 3]] - listSD2Matr[[maxSecondPos[[j]] - 3]]) < 0.1) & (abs(listSDMatr[[maxFirstPos[[i]] + 3]] - listSD2Matr[[maxSecondPos[[j]] + 3]]) < 0.1) & (abs(listSDMatr[[maxFirstPos[[i]] - 4]] - listSD2Matr[[maxSecondPos[[j]] - 4]]) < 0.1) & (abs(listSDMatr[[maxFirstPos[[i]] + 4]] - listSD2Matr[[maxSecondPos[[j]] + 4]]) < 0.1) & (abs(listSDMatr[[maxFirstPos[[i]] - 5]] - listSD2Matr[[maxSecondPos[[j]] - 5]]) < 0.1) & (abs(listSDMatr[[maxFirstPos[[i]] + 5]] - listSD2Matr[[maxSecondPos[[j]] + 5]]) < 0.1)){
      posPairs[[a]] <- maxFirstPos[[i]]
      posPairsVal[[a]] <- maxFirst[[i]]
      a <- a + 1
      posPairs[[a]] <- maxSecondPos[[j]]
      posPairsVal[[a]] <- maxSecond[[j]]
      a <- a + 1
      
    }
     
  }
  
}

#Параметры нового изображения 
minVal1 <- posPairs[[1]]
minVal2 <- posPairs[[2]]
minLast <- posPairs[[2]]
minSumPos <- 0
for(i in seq(1, 40, 2)){
  if(posPairs[[i + 1]] < minLast & posPairs[[i]] > 0)
  {
    minSumPos <- i
    minVal1 <- posPairs[[i]]
    minVal2 <- posPairs[[i + 1]]
    minLast <- posPairs[[i + 1]]
  }
  newSizeF <- h1 * (minVal1 %/% 40 + 1)
  newSizeS <- (h2 * (minVal2 %/% 40 + 1))               
  newSizeAll <- newSizeF + newSizeS
  
  img <- image_read(location)
  img2 <- image_read(location2)
  
  #Размеры
  imgSzF <- paste(newSizeF,"x",hF, sep="")
  imgSzS <- paste(hS,"x",wS,"+",newSizeS, sep="")
  
  newMatrixF <- image_crop(img, imgSzF)
  newMatrixS <- image_crop(img2, imgSzS)
  image_write(newMatrixF, path = "new1.jpg", format = "jpg")
  image_write(newMatrixS, path = "new2.jpg", format = "jpg")
}








