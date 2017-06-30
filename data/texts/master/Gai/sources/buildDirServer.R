# формирование списка каталогов для сервера
# сгенерировать новое имя для каждого изображения: добавить в начало число от 1 до ...
# количества изображений во всех папках
# потом надо все изображения в одну папку скопировать.

# вообщем этот скрипт делает имена изображений уникальными и копирует их все в одну папку

# исходный каталог
prefserv <- 'd:/Temp/'
prefserv <- '/home/vgai/images/'

# результирующий каталог
# resultdir <- 'd:/Temp/result/'
resultdir <- '/home/vgai/result/'

# количество каталогов 'images'
Ndir <- 50

dirnames <- matrix('', 1, Ndir)

for (i in 1:Ndir)
{
  dirnames[i] <- paste(prefserv, i, '/', sep = "")
  print(dirnames[i])
  
  # определяем количество изображений в каталоге
  #flsDir <- list.files(dirnames[i], pattern = "\\.jpg$", ignore.case = TRUE)
}

# переименовываем изображения
ind <- 0
for (i in 1:Ndir)
{
  # определяем количество изображений в каталоге
  flsDir <- list.files(dirnames[i], pattern = "\\.jpg$", ignore.case = TRUE)
  
  for (j in 1:length(flsDir))
  {
    newname <- paste(resultdir, ind, flsDir[j], sep = "")
    oldname <- paste(dirnames[i], flsDir[j], sep = "")
    file.copy(oldname, newname)
    print(newname)
    print(oldname)
    ind <- ind + 1
  }
}


# теперь попробуем выбирать данные нужного размера
# индексы для изображений, добавляемых в базу лучше с самого начала
# сохранить в одно место и просто потом использовать.
# надо это будет доделать после того, как определим общее число изображений

# генерации 10 уникальных чисел из 1..100
sort(sample.int(100, 10))

