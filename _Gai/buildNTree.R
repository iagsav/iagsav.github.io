# построение дерева N-мерного
    
    # function to build N-ary tree
    makeTree <- function(depth, leafAmn)  
    {
    ## leafAmn - branching factor
    ## depth   - depth of tree
    library(data.tree)

    myTree <- Node$new("root", myData = c(-1)) # root node
    for (i in 1:depth) # loop by tree depth
    {
        if (i == 1)
        # create a set of nodes with depth 1
        {
            chldArr1 <- matrix("", 1, leafAmn)
            for (j in 1:leafAmn)
            {
                # create children nodes
                myTree$AddChild(j, myData = c())
                # save links to children nodes to array 'chldArr1'
                # this array is used to generate tree without using recursion
                chldArr1[j] <- sprintf("myTree$children[[%d]]$", j)
            }
        }
        else
        # add childs at level 'i' to nodes at level 'i-1' using 
        # method AddChild
        {
            chldArr2 <- matrix("", 1, (leafAmn ^ i))
            k <- 1
            for (j in 1:(leafAmn ^ (i - 1)))
            {
                for (m in 1:leafAmn)
                {
                    # create string which contains a command to execute
                    # this command is used to add child to nodes at previous level
                    commStr <- paste(chldArr1[j], sprintf("AddChild(%d, myData = c())", m), sep = "")
                    eval(parse(text = commStr))
                    print(commStr)
                    # save command to array 'chldArr2'
                    chldArr2[k] <- paste(chldArr1[j], sprintf("children[[%d]]$", m), sep = "")
                    k <- k + 1
                }
            }
            chldArr1 <- chldArr2
        }
    }
    
    # crete formatter to print the myData field
    SetFormat(myTree, "myData", formatFun = function(x) paste(x, collapse = " "))

    return(myTree)
    }
    
    ## Make a tree with depth of '2' and 3 branches from each node
    imTree <- makeTree(6, 30)
    print(myTree, "myData")
    

# вообще очень интересно получаетс. вот дл первой модели. сколько уровней? мне 
# кажетс не больше трёх. т.е. 30 - 900 - 27000. и то. как-то странно это. но пусть будет 
# так.

# Hello! How can I build an N-ary tree in R?    
    
# Building N-ary tree
# Hello! I want to present the solution, which i used to build a tree data structure with *N* branching factor (parameter **leafAmn**). 
# To store data in the tree the field **myData** is used. Also function to print the content of the tree is defined.
# Also it exists a recursive solution of this task:
# http://stackoverflow.com/questions/33768841/r-tree-with-n-branches