imread = function(x)
# функция для проверки является ли файл изображением или нет
{
#   library(bmp)
#   library(png)
#   library(jpeg)
#   library(caTools)
#   library(tiff)
  
  a <- tryCatch(readJPEG(x),
                error = function(x)
                {
                  1;
                })
  
  if (length(a) > 1)
  {
    return(a);
  }
  
  if (a == 1)
  {
    a <- tryCatch(readPNG(x),
                  error = function(x)
                  {
                    2
                  })
  }
  
  if (length(a) > 1)
  {
    return(a);
  }  
  
  if (a == 2)
  {
    a <- tryCatch(read.bmp(x),
                  error = function(x)
                  {
                    3
                  })
  }  
  
  if (length(a) > 1)
  {
    return(a);
  }
  
  if (a == 3)
  {
    a <- tryCatch(read.gif(x),
                  error = function(x)
                  {
                    4
                  })
  }  
  
  if (length(a) > 1)
  {
    return(a);
  }
  
  if (a == 4)
  {
    a <- tryCatch(read.tiff(x),
                  error = function(x)
                  {
                    5
                  })
  }  
  
  if (length(a) > 1)
  {
    return(a);
  }
  
  if (a == 5)
  {
    # тут всё плохо и не один формат не подошёл
    # ??? удалять ???
    return(-1);
  }
  
  # наверное ещё нужно тип возвращать.  
  
}