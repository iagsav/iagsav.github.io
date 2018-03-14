# тут будем проверять работу с деревом

birds <- Node$new("root") # Aves - имя
birds$AddChild("Neognathae", vulgo = "New Jaws", species = c(2,2,3))
birds$AddChild("Palaeognathae", vulgo = "Old Jaws", species = c(1,2,3))

SetFormat(birds, "species", formatFun = function(x) paste(x, collapse = " "))

print(birds, "vulgo", "species")

imTree <- Node$new("root", myData = c(-1)) 
imTree$AddChild("1", myData = c())
imTree$AddChild("2", myData = c(1,2,3))
imTree$children[[1]]$AddChild("1", myData = c(2,2,10,11))
imTree$children[[1]]$children[[1]]$AddChild("1", myData = c(100, 200))

SetFormat(imTree, "myData", formatFun = function(x) paste(x, collapse = " "))

print(imTree$children[[1]]$myData)
print(imTree, "myData")

N <- 3

leaf <- 30
imTree1 <- Node$new("root", myData = c(-1)) 
for (i in 1:3)
{
  if (i == 1)
  {
    sarr <- matrix("", 1, leaf)
    for (j in 1:leaf)
    {
      imTree1$AddChild(j, myData = c())
      sarr[j] <- sprintf("imTree1$children[[%d]]$", j)
    }
  }
  else
  {
    sarr2 <- matrix("", 1, (leaf ^ i))
    k <- 1
    for (j1 in 1:(leaf ^ (i - 1)))
    {
        for (j in 1:leaf)
        {
        tmpstr <- paste(sarr[j1], sprintf("AddChild(%d, mydata = c())", j), sep = "")
        eval(parse(text = tmpstr))
        print(tmpstr)
        sarr2[k] <- paste(sarr[j1], sprintf("children[[%d]]$", j), sep = "")
        
        k <- k + 1
      }
    }
    sarr <- sarr2
  }
}

SetFormat(imTree1, "myData", formatFun = function(x) paste(x, collapse = " "))
print(imTree1, "myData")


# вообще очень интересно получается. вот для первой модели. сколько уровней? мне 
# кажется не больше трёх. т.е. 30 - 900 - 27000. и то. как-то странно это. но пусть будет 
# так.