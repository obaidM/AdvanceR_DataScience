#  shuffle data (randomize)
#
###
#
# Several ways but they all rely on random numbers so
#
rm(list=ls(all=TRUE))
seed <- 1

###
#
# use sample()
#
# default is replace=F (sampling without replacement)
#
#
set.seed(seed)

( t <- 1:10 )
( s <- sample(1:length(t)) )

###
#
# do it explicitly ( the parens make it look harder than it is )
#
# note: results differ from sample()
#
#
set.seed(seed)

( t <- 1:10 )
( s <- t[order(runif(length(t)))] )

###
#
# draw a random subset
#
# note: first "n" elements from the earlier use of sample()
#
set.seed(seed)

n   <- 5        # size of sample
( t <- 1:10 )
( s <- sample(t, n) )

# specify as a fraction rather than a count (at least initially)
set.seed(seed)
( pctest <- 0.5 )
( frac   <- ceiling(nrow(t)*pctest ))
  ( s      <- sample(t, frac)        )
  
  ###
  #
  # divide data into multiple groups
  #
  # note: works easiest if all groups are the same size
  #
  #
  ##
  # example: 10 items in 2 groups
  n  <- 10
  g  <- 2
  rep( 1:2, each=(n/g) )
  
  # 10 items, 3 groups
  n  <- 10
  g  <- 3
  ( t <- rep( 1:g, each=ceiling(n/g) ) ) # generates 12 entries rather than 10
  ( t <- t[1:n] )                        # keep the first 10 entries, last group is smaller
  
  ###
  #
  # another function: sample_n() [ dplyr, included in tidyverse ]
  #
  #   sample_n()     user specified observation count
  #   sample_frac()  user specified fraction of total observations
  #
  # note: recognizes group_by() and works within groups
  #       works in tidyverse environment
  
  require(tidyverse)
  
  ( t <- crossing(a=1:3, b=4:6) )
  t %>% group_by(a) %>% sample_n(2)
  
  ###
  #
  # example: jackknife estimates of mean() [ LOOCV ] See ISLR section 5
  #
  #
  n  <- 50
  t  <- 1:n
  x  <- rep(NA, n)
  
  for ( i in 1:n ) {
    t2   <- t[ -i ]          # negative index indicates exclusion, generalizes to vector
    x[i] <- mean(t2) 
  }
  x
  
  # examples
  
  mean(t)
  mean( ( x-mean(t) )^2 )      # MSE (mean squared error)
  sd(   ( x-mean(t) )^2 )      
  
  ( d    <- x - mean(t) )
  
  ( ci95 <- c(0.025, 0.975) )
  quantile(d, probs=ci95) # 0.95 CI 
  
  ( t    <- quantile(d, probs=0.5 ) ) # median
  ( ecdf(d) (t) )                     # ecdf() to find quantile() of "t" [ does not always work well when data is sparse ]
  