library('bmp') 
library('jpeg') 
location <- readLines("path.txt")
img <- read.bmp(location)

#Перевод в серое
if (length(dim(img)) == 3)
{
  img <- img[,,1] + img[,,2] + img[,,3];
  img <- img / 3;
}  
#Нормализация
img <- img - min(img)
img <- img / max(img)

round2 = function(x, n) {
  posneg <- sign(x)
  z <- abs(x)*10 ^ n
  z <- z + 0.5
  z <- trunc(z)
  z <- z/10 ^ n
  return(z*posneg)
}

sz <- dim(img) #задали размер матрицы
h <- sz[2]
w <- sz[1]
imgFirstSH <- h
imgFirstSW <- w
imgFirstSizeH <- h - h %% 10
imgFirstSizeW <- w - w %% 10
h <- imgFirstSizeH
w <- imgFirstSizeW

#Фильтр
filt1<-matrix(c(1,1,-1,-1),nrow = 4,ncol=4)
#Значения для разделения на подобласти
shAll = c(1, round2((1/10) * h, 0), round2((1/10) * h, 0) + 1, round2((2/10) * h, 0), round2((2/10) * h, 0) + 1, round2((3/10) * h, 0), round2((3/10) * h, 0) + 1, round2((4/10) * h, 0), round2((4/10) * h, 0) + 1, round2((5/10) * h, 0), round2((5/10) * h, 0) + 1, round2((6/10) * h, 0), round2((6/10) * h, 0) + 1, round2((7/10) * h, 0), round2((7/10) * h, 0) + 1, round2((8/10) * h, 0), round2((8/10) * h, 0) + 1, round2((9/10) * h, 0), round2((9/10) * h, 0) + 1,h)
swAll = c(1, round2((1/10) * w, 0), round2((1/10) * w, 0) + 1, round2((2/10) * w, 0), round2((2/10) * w, 0) + 1, round2((3/10) * w, 0), round2((3/10) * w, 0) + 1,round2((4/10) * w, 0), round2((4/10) * w, 0) + 1,round2((5/10) * w, 0), round2((5/10) * w, 0) + 1,round2((6/10) * w, 0), round2((6/10) * w, 0) + 1,round2((7/10) * w, 0), round2((7/10) * w, 0) + 1,round2((8/10) * w, 0), round2((8/10) * w, 0) + 1,round2((9/10) * w, 0), round2((9/10) * w, 0) + 1, w)

#Размер каждой подобласти
saveSh <- shAll[2]
saveSw <- swAll[2]


listMass<-list(0)#Лист для подобластей начальных
listms<-list(0)#Лист для 
mass <- matrix(0, 4, 4)

for(i in 1:100)
{
  listMass[[i]]<-list(matrix(0, nrow=saveSw,ncol=saveSh))#Задали лист для разделение img на матрицы
}

for(i in 1:100)
{
  listms[[i]]<-list(matrix(0, nrow=4,ncol=4))#Задали лист для каждой матрицы 4 на 4
}

j1 <- 1
k1 <- 1
for (j in seq(1,20,2))
{
  for (i in seq(1,20,2))
  {  
    dtFirst <- img[swAll[i]:swAll[i + 1], shAll[j]:shAll[j + 1]]
    listMass[[j1]] <- list(dtFirst)#Разделили img
    j1 = j1 + 1
  }
}
#Работа с каждой матрицей по-отдельности
for(i in 1:100){
  j1 <- 1
  k1 <- 1
  mass <- matrix(0, 4, 4)
  #задали размер матрицы
  h1 <- saveSh
  w1 <- saveSw
  
  listProm<-matrix(unlist(listMass[i]),nrow = w1, ncol = h1)
  
  sh = c(1, round2((1/4) * h1, 0), round2((1/4) * h1, 0) + 1, round2((2/4) * h1, 0), round2((2/4) * h1, 0) + 1, round2((3/4) * h1, 0), round2((3/4) * h1, 0) + 1, h1)
  sw = c(1, round2((1/4) * w1, 0), round2((1/4) * w1, 0) + 1, round2((2/4) * w1, 0), round2((2/4) * w1, 0) + 1, round2((3/4) * w1, 0), round2((3/4) * w1, 0) + 1, w1)
  
  
  
  for (j in seq(1,8,2))
  {
    for (k in seq(1,8,2))
    {  
    #сокращаем до матрицы 4 на 4
      dt <- listProm[sw[j]:sw[j], sh[k]:sh[k]]
      mass[k1,j1] = sum(dt)
      listms[[i]] <- list(mass)
      j1 = j1 + 1
    }
    
    j1 = 1
    k1 = k1 + 1
  } 
  
  
}


spectr <- list(0)
spectrFin<-matrix(0, 10, 10)
for(j in 1:100)
{ 
  
  spectr[j]<-sum(filt1*unlist(listms[[j]]))#Получение листа спектральных коэф
  
}

spectrFin<-matrix(spectr,nrow=10,ncol=10) #Перевод в матрицу
#Ищем максимальные ячейки
listNumber<-array(0,2)
firstCoord<-array(0,5)
secondCoord<-array(0,5)
listNumber[[1]] <- 0
listNumber[[2]] <- 0
#listNumber[[3]] <- spectrFin[[1]]
#Ищем максимальные значения матрицы спектральных коэффицентов две штуки, том ищем соседние
listP<-list(spectr)
spectrFin2<-spectrFin

for(j in 2:5){
 
  for(i in 1:100){
  if((spectrFin2[[i]] > listNumber[[1]]) & i > 20 & i < 90 & (i %% 10) > 2 & (i %% 10) < 10 & spectrFin2[[i]] > 0.75)
  {
    
    if((spectrFin2[[i + 10]] > 0.75 | (spectrFin2[[i - 10]] > 0.75)))
    {
    if(j > 2 & firstCoord[[j - 1]] != 0 & ((i %% 10) > (firstCoord[[j - 1]] %% 10)))
     {
      listNumber[[1]] <- spectrFin2[[i]]
      firstCoord[[j]] <- i
    }
    if(j == 2){
    listNumber[[1]] <- spectrFin2[[i]]
    firstCoord[[j]] <- i
    }
    }
    }
  }
 
  
  #Поиск соседних значений
  for(i in 1:100){
    if(spectrFin2[[i]] > listNumber[[2]] & i > 20 & i < 90 & ((i %% 10) > 2) & (i %% 10) < 10 & (spectrFin2[[i]] < listNumber[[1]]) & ((i %/% 10 == firstCoord[[j]] %/% 10 + 1) | (i %/% 10 == firstCoord[[j]] %/% 10 - 1)) & (i %% 10 == firstCoord[[j]] %% 10) & spectrFin2[[i]] > 0.75)
    {
      if(j > 2 & ((i %% 10) > (secondCoord[[j - 1]] %% 10))){
        listNumber[[2]] <- spectrFin2[[i]]
        secondCoord[[j]] <- i
      }
      if(j == 2){
        listNumber[[2]] <- spectrFin2[[i]]
        secondCoord[[j]] <- i
      }
      
    }
    
  }


  if(firstCoord[[j - 1]] > 0){
  spectrFin2[[firstCoord[[j - 1]]]]<-0
  }
  if(secondCoord[[j - 1]] > 0){
  spectrFin2[[secondCoord[[j - 1]]]]<-0
  }
  listNumber[[1]] <- 0
  listNumber[[2]] <- 0
  
}

#находим большие чтобы расставить координаты по порядку
coordA<-firstCoord[[2]] 
coordB<-secondCoord[[2]] 
for(i in 2:5){
if(firstCoord[[i]] %% 10 > coordA %% 10)
{
    coordA<-firstCoord[[i]]
}
if(secondCoord[[i]] %% 10 > coordB %% 10)
{
  coordB<-secondCoord[[i]]
}
}
#меняем координаты местами, меньшая - А, слева, большая-В, справа.
if(coordA > coordB){
  
  middleVal <- coordB
  coordB <- coordA
  coordA <- middleVal
}



#координаты номера
#Верхняя левая точка прямоугольника
LeftTopX <- saveSh * (coordA %/% 10)
LeftTopY <- saveSw * (coordA %% 10 - 1) - (saveSw / 3)
#Нижняя Левая
LeftBottomX <- saveSh * (coordA %/% 10)
LeftBottomY <- saveSw * (coordA %% 10) - (saveSw / 2)
#Верхняя правая точка прямоугольника
RightTopX <- saveSh * (coordB %/% 10) + saveSw
RightTopY <- saveSw * (coordB %% 10 - 1) - (saveSw / 3)
#Нижняя правая
RightBottomX <- saveSh * (coordB %/% 10) + saveSw
RightBottomY <- saveSw * (coordB %% 10) - (saveSw / 2)
#Усредняем если рядом есть положительные значения(на случай если рамка в 3х ячейках, а не двух)
if(spectrFin[[coordA - 10]] >= (spectrFin[[coordB + 10]] * 3)){
  LeftTopX <- (saveSh * (coordA %/% 10)) - (saveSh/2)
  LeftBottomX <- (saveSh * (coordA %/% 10)) - (saveSh/2)
  RightTopX <- (saveSh * (coordB %/% 10) + saveSw) - (saveSh/2)
  RightBottomX <- (saveSh * (coordB %/% 10) + saveSw) - (saveSh/2)
}
if(((spectrFin[[coordA - 10]] * 3) <= spectrFin[[coordB + 10]])){
  LeftTopX <- (saveSh * (coordA %/% 10)) + (saveSh/2)
  LeftBottomX <- (saveSh * (coordA %/% 10)) + (saveSh/2)
  RightTopX <- (saveSh * (coordB %/% 10) + saveSw) + (saveSh/2)
  RightBottomX <- (saveSh * (coordB %/% 10) + saveSw) + (saveSh/2)
}
if(spectrFin[[coordA - 10]] > 0 & spectrFin[[coordB + 10]] > 0 & (spectrFin[[coordB + 10]]/spectrFin[[coordA - 10]]) < 3 & (spectrFin[[coordB - 10]]/spectrFin[[coordA + 10]]) < 3){
  LeftTopX <- (saveSh * (coordA %/% 10)) - (saveSh/6)
  LeftBottomX <- (saveSh * (coordA %/% 10)) - (saveSh/6)
  RightTopX <- (saveSh * (coordB %/% 10) + saveSw) + (saveSh/6)
  RightBottomX <- (saveSh * (coordB %/% 10) + saveSw) + (saveSh/6)
}


#Рисуем номерную область на фото
a <- 1
b <- 1
for(i in 1:(h*w)){
  
  if((i > ((((LeftTopX%/%1) - 1) * imgFirstSW) + LeftTopY) & i < ((((LeftTopX%/%1) - 1) * imgFirstSW) + (LeftBottomY%/%1)))  | (i > (((LeftTopX%/%1) * imgFirstSW) + (LeftTopY%/%1)) & i < (((LeftTopX%/%1) * imgFirstSW) + (LeftBottomY%/%1)))){
    img[[i]] <- 6
    
  }
  if((i > ((((RightTopX%/%1) - 1) * imgFirstSW) + (RightTopY%/%1)) & i < ((((RightTopX%/%1) - 1) * imgFirstSW) + (RightBottomY%/%1))) | (i > (((RightTopX%/%1) * imgFirstSW) + (RightTopY%/%1)) & i < (((RightTopX%/%1) * imgFirstSW) + (RightBottomY%/%1)))){
    img[[i]] <- 6
  }
  if((i > ((((LeftTopX%/%1) + a) * imgFirstSW) + (LeftTopY%/%1)) & i < ((((LeftTopX%/%1) + a) * imgFirstSW) + (LeftTopY%/%1) + 4) & i < (((RightTopX%/%1) * imgFirstSW) + (LeftTopY%/%1) + 4))){
    img[[i]] <- 6
    a <- a + 1
  }
  if((i > ((((LeftBottomX%/%1) + b) * imgFirstSW) + (LeftBottomY%/%1) - 2) & i < ((((LeftBottomX%/%1) + b) * imgFirstSW) + (LeftBottomY%/%1)) & i < (((RightBottomX%/%1) * imgFirstSW) + (LeftBottomY%/%1)))){
    img[[i]] <- 6
    b <- b + 1
  }
}

kek <- str(location)
  
#вывод изображения
writeJPEG(img, target = "NewImage.jpg")

