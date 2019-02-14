rm(list=ls(all=TRUE))
###
# scope of variables
#

x <- 6  # basic example
x

sfn <- function() { # silly function
  x <- 4              # assign a value to local instance
  print(x)            # print local instance
  print(x+1)          # print result of opertion on local instance
}

sfn()
x

sfn2 <- function() { # another silly function
  x <- 4               # assign a value to local instance
  print(x)             # print local instance
  print(x+1)           # print result of operation on local instance
  print(y)             # print non-local (i.e. global) variable
}

y <- 10
