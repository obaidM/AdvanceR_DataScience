rm(list = ls())
#
# bootstrap index generation and use
#
# note: broom and dplyr are moving away from bootstrap() support
#   and they appear to be getting rid of do() [?]
#
#   Wickham wrote some VERY slick code which I encountered at 
#
#      https://github.com/tidyverse/dplyr/issues/269 
#
#   and experimented with (the line for "indicies" does most of the
#   heavy lifing and is quite subtle - or at least it was for me, exploring
#   and experimenting with code is always interesting and should be encouraged)
#
#   anyhow, this is some basic how-to code snippits for 2 kinds of bootstrap operations
#
#
###
#
#  History
#
#     20181024 wj Initial code
#
###
#
# generally useful values (can be changed as desired)
#

seed  <- 1 # horrible seed value but serves our purpose here

###
# 
# multiple copies a sequence
#
#
my.seq <- 1:2  # keep it short so you can follow what is happening and get comfortable with the results
n      <- 3

rep(my.seq, n)

###
#
# generate "b" copies of random indicies for an original sample of size "n" (with replacement)
#
#  this is the basis for classic bootstrap operation: generate a collection of collections of indexs values
#
b <- 3 # number of bootstrap index sequences
n <- 4 # size of original sample

set.seed(seed)

t <- replicate(b, sample(n, replace=T), simplify=F)  # borrowed from tidyverse link w/ great reverence
t

class(t)

t1 <- t[[1]]  # first element of the list (un-named)
t1

###
#
# generate "b" copies of random indicies for an original sample size "n" (with replacement)
#
#  AND make sure each index value occurs the same number of times across the 
#      collection of "b" copies
#
# "balanced" boostrap (see: Davison, A. Schechtman, E., "Efficient bootstrap simulation",
#     Biometrika, 73, 3, 555-566 (1986) )
#
# COMMENT:  this code is VERY basic but it WORKS (first criteria)
#           clearer, faster code would be welcomed
#
#

b <- 3  # number of bootstrap samples
n <- 4  # number of original observations

set.seed(seed)

i <- sample(rep(1:n, b)) # generate and shuffle, work this in parts if needed, to be sure you see it
g <- rep(1:b, each=n)    # define the sample boundaries

table(i)

i
g

# how to use it
for (j in 1:max(g) ) {
  k <- i[ g == j ]    # index values for sample
  print (j)
  print (k)
}

###
#
# other code to make this faster (cleaner and faster)
#

b <- 3 # number of bootstrap samples
n <- 4 # number of original observations

set.seed(seed)  ###### USE THIS PART 

i <- sample(rep(1:n, b)) # generate and shuffle, work this in parts if needed, to be sure you see it
g <- matrix(data=i, ncol=n, byrow=T)

# how to use it
for (j in 1:nrow(g) ) {
  k <- g[ j, ]
  print (j)
  print (k)
}

## feed the lm to the loop
## tidy the summary result 
## use rbind to collect the results 
## use group by and summarize to get the results
###
#
# bootstrap: explicit "train" v. "test" although you can generate "test" for each sample
#
#
# check use of parameter "subset" in lm(), glm(), nls() 
#