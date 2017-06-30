# скрипт дл формировани дерева по откликам фильтров, от которых
# потом перешли к операторам

setwd('d:\\Dropbox\\repository\\Rassmem\\linux\\good\\new\\')

# загрузили библиотеку
library(data.tree)
modeltype <- 4

# чтобы построить дерево будем сначала считывать описание, а потом добавлть описание в дерево
# причёv  знаю сколько в дереве будет уровней N+1 (N = 6)

# сначала нужно выбрать файлы с заданным окончанием

#procdir <- '/home/vgai/images/tmp/'
procdir <- '/home/vgai/result/'
#fls <- list.files(procdir, pattern = "\\FL.rdata$", ignore.case = TRUE)
load('flsdec.rdata')
fls <- flsdec

# выбираем случайные индексы

# префикс файла модели, зависит от числа добавляемых в модель файлов
# modelPref

N <- 3 # 3?

# хот сначала наверное надо структуру дерева сделать
# перед тем как его заполнть

# загрузка дерева с заданной структурой
load('imTree30.rdata')

#load('U1840379.jpgFL.rdata')


# цикл по файлам
for (i in 1:length(fls))
{
  rind <- c()
  iprn <- -1  
  
  # загрузка данных
  load(paste(procdir, fls[[i]], sep = ""))
  
  # цикл по уровнм
  pref <- 'imTree$'
  for (j in 1:N)
  {
    
    if (j == 1)
    {
      myind <- 1:length(dec[[j]])
    }
    else {
      myind <- (iprn*4 - 3):(iprn*4)      
    }
    # определем максимум на j-ом уровне по каким-то своим признакам
    tmp <- matrix(0, length(dec[[j]]), 16)
    
    for (k in myind)
    {
      tmp[k,] <- dec[[j]][[k]][[1]][1:16]
    }

    imax <- which(abs(tmp) == max(abs(tmp)), arr.ind = TRUE)
    if (prod(dim(imax)) > 2)
    {
      print(fls[[i]]) 
      imax <- imax[1,]
      imax <- as.matrix(imax)
    }

    if (prod(dim(imax)) == 2)
    {
      # print(fls[[i]]) 
      imax <- as.matrix(imax)
    }

    # imax[1] - номер области на j-ом уровне
    # выбрали область. теперь надо выбрать в этой области максимальный оператор
    iprn <- imax[1]
    
    tmp <- tmp[imax[1], 1:15]
    
    imax <- which(abs(tmp) == max(abs(tmp)), arr.ind = TRUE)
    
    iind <- imax[1] # imax[2] - номер оператора
    
    if (tmp[iind] < 0)
    {
      iind <- iind + 15
    }

    # iind - адрес узла на i-ом уровне разложени
    # теперь опть надо мутить со строками 
    pref <- paste(pref, sprintf('children[[%d]]$', iind), sep = "")
    
    rind <- c(rind, iind)
    
    # тут формируем команду, котора добавлет в нужную координату
    commStr <- paste(pref, "myData", sep = "")
    commStr <- paste(commStr, " <- c(", commStr, ",", i, ")", sep = "")
    #print(commStr)
    # и выполнем команду
    eval(parse(text = commStr))
  }
  #print(rind)
  
}

save(imTree, file = paste('model_', modeltype, '.rdata', sep = ''))