###
#
# Simulate data (5 clusters training, 6 clusters testing)
#
# use mahalanodis distance to check cluster membership probability in testing set
#
###
#

rm(list=ls(all=TRUE))
library(tidyverse)
library(broom)

set.seed(1)

# training set characteristics (group labels, group sizes, and means for each component variable)
centers <- data.frame( grps  = 1:5,
                       gsize = c(1000, 500, 750, 900, 800),
                       m1    = c(  -2,  -1,   0,   1,   2),
                       m2    = c(   0,   3,   1,   2,   4),
                       m3    = c(   1,   4,   2,   5,  -1),
                       m4    = c(   2,  -3,   4,  -1,   1) )

# testing set characteristics (same structure as training set)
c2      <- data.frame( grps  = 1:6,
                       gsize = c( 100, 100, 100, 100, 100, 100),
                       m1    = c(  -2,  -1,   0,   1,   2,   4),
                       m2    = c(   0,   3,   1,   2,   4,   6),
                       m3    = c(   1,   4,   2,   5,  -1,   7),
                       m4    = c(   2,  -3,   4,   -1,  1,   8) )

# training set generation  and traing  data is ready. 
kd      <- centers        %>%
  group_by(grps) %>%
  do(data.frame( v1= rnorm(.$gsize[1], .$m1[1]),
                 v2= rnorm(.$gsize[1], .$m2[1]),
                 v3= rnorm(.$gsize[1], .$m3[1]),
                 v4= rnorm(.$gsize[1], .$m4[1])) ) 

# testing set generation (note differences in structure)
kd2     <- c2             %>%
  group_by(grps) %>%
  do(data.frame( v1= rnorm(.$gsize[1], .$m1[1]),
                 v2= rnorm(.$gsize[1], .$m2[1]),
                 v3= rnorm(.$gsize[1], .$m3[1]),
                 v4= rnorm(.$gsize[1], .$m4[1])) ) 

# kmeans for 1 to 10 clusters (training set) [ caution: does not scale well  to larger set of data ]
kclust  <- kd                %>%   ### taking training set
  crossing(k= 1:10) %>%              ## probably comes out of broom package
  group_by(k)       %>%
  do(clust= kmeans(select(., v1, v2, v3, v4), .$k[1], nstart=5) )

# extract kmeans results information
clusters    <- kclust  %>% tidy(clust)
assignments <- kclust  %>% augment(clust, kd)
clusterings <- kclust  %>% glance(clust)

plot(clusterings$k, clusterings$tot.withinss)

# cluster assignments for 5 cluster case (based on plot)
t           <- kclust$clust[5]
t2          <- t[[1]]
tmp         <- kd
tmp$cluster <- t2$cluster

# compare original and discovered clusters; extra parenthesis gets result printed immediately
(tbl         <- table(tmp$grps, tmp$cluster) )

# table results:
#
#   
#      1   2   3   4   5
#  1 934   1   2   2  61
#  2   0  23 477   0   0
#  3  38   2   0   3 707
#  4   0 839  60   0   1
#  5   2   1   1 793   3
#
# these are hard-coded
pc_correct <- (934+477+707+839+793)/3950
# [1] 0.9493671
#
# algorithmically: (sum of row max)/ total give pc_correct
sum(apply(tbl,1,max))/sum(tbl)

# prep for mahalanobis distance [ updated ]
# uses scale() to get means and std.dev; no loops
variability <- function(df) {
  df$cluster <- NULL
  df$grps    <- NULL
  n          <- nrow(df)
  df2        <- scale(df, center=T, scale=T)  # convert to Z w/ local mean and std.dev not needed if 
  sscp       <- t(df2) %*% df2                # X'X
  vcvinv     <- solve((1/(n-1)) * sscp)       # inverse of variance-covariance matrix
  return( list(n      = n,
               avg    = attr(df2, "scaled:center"),
               sdev   = attr(df2, "scaled:scale"),
               vcvinv = vcvinv )
  )
}

# calculate and retain summary information for each existing cluster
mhWork   <- tmp                       %>%
  group_by(cluster)         %>%
  do( desc=variability(.) )

class(mhWork)
print(mhWork)
############## The desc of the clustering model is ready 
clusters <- mhWork$cluster
desc     <- mhWork$desc       ####### this is the info of the cluster. This is what we work with

mhDf     <- length(desc[[1]]$avg)             # degrees of freedom for chi-sq

###
# calculate mahalanobis distances for training set to check behavior
###
# storage space for distances
d.train  <- matrix( -1,
                    nrow=nrow(kd),
                    ncol=length(clusters) )

# clear out "grps", but hold onto it for later use
kdGrps   <- kd$grps
kd$grps  <- NULL

# collect Mahalanobis distance for training data across clusters
for ( i in seq_along(clusters) ) {
  t            <- desc[[i]]
  tdf          <- scale(kd, center=t$avg, scale=t$sdev)   # scale to Z w/ orig. cluster dist
  d.train[, i] <- mahalanobis(tdf, center=F, cov=t$vcvinv, inverted=T)
}

# which cluster for each testing observation?
newClust    <- apply(d.train, 1, which.min)  ## apply for each row
kd$cluster <- newClust
kd$grps    <- kdGrps
table(kd$grps, kd$cluster)

# chi-squared cdf value for each testing observation
minStat  <- apply(d.train, 1, min)
chiSq    <- pchisq(minStat, df=mhDf, lower.tail=F)
summary(chiSq)

kd$minStat   <- minStat
kd$chiSq     <- chiSq

# distribution of p-value (tail area) by group for testing sample
boxplot(chiSq~grps,data=kd)

hist(kd$chiSq)

###
# calculate mahalanobis distances for testing set to compare behavior
###
# storage space for distances
d2       <- matrix( -1,
                    nrow=nrow(kd2),
                    ncol=length(clusters) )

# hold on to "grps" info for accuracy check but remove from kd2 structure
kd2Grps  <- kd2$grps
kd2$grps <- NULL

# collect Mahalanobis distance for test data across clusters [ updated ]
for ( i in seq_along(clusters) ) {
  t       <- desc[[i]]
  tdf     <- scale(kd2, center=t$avg, scale=t$sdev)   # scale to Z w/ orig. cluster dist
  d2[, i] <- mahalanobis(tdf, center=F, cov=t$vcvinv, inverted=T)
}

# which cluster for each testing observation?
newClust    <- apply(d2, 1, which.min)
kd2$cluster <- newClust
kd2$grps    <- kd2Grps
table(kd2$grps, kd2$cluster)

# chi-squared cdf value for each testing observation
minStat  <- apply(d2, 1, min)
chiSq    <- pchisq(minStat,df=mhDf, lower.tail=F)
summary(chiSq)

kd2$minStat   <- minStat
kd2$chiSq     <- chiSq

# distribution of p-value (tail area) by group for testing sample
boxplot(chiSq~grps,data=kd2)

hist(kd2$chiSq)

