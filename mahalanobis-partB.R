###
# HW 4 reference code
#

# print related option (to avoid scientific notation)
rm(list=ls(all=TRUE))
options("scipen"=100)

# libraries
library(tidyverse)
library(broom)

# my working area: 

load(file="hw4_desc.RData")          # MhD values from training set
load(file="hw4_newData.RData")       # new data to be segmented and evaluated

kd2      <- newData                  # just to keep from needing to re-write the rest of the code
mhDf     <- length(desc[[1]]$avg)    # degrees of freedom for chi-sq

clusters <- length(desc)             # number of clusters

# pre-allocate space for the distance measurements
d2       <- matrix(-1, nrow=nrow(newData), ncol=clusters)

# collect Mahalanobis distance for test data across clusters [ updated ]
for ( i in 1:clusters ) {
  t       <- desc[[i]]
  tdf     <- scale(kd2, center=t$avg, scale=t$sdev)                 # scale to Z w/ orig. cluster parameters
  d2[, i] <- mahalanobis(tdf, center=F, cov=t$vcvinv, inverted=T)   # distances by row to existing cluster "i"
}

# which cluster for each testing observation?
newClust    <- apply(d2, 1, which.min)                   # which cluster
minStat     <- apply(d2, 1, min)                         # which value
chiSq       <- pchisq(minStat, df=mhDf, lower.tail=F)    # size of rejection region
#summary(chiSq)

#hist(chiSq)                    # visual inspection

# create the deliverable
newDist             <- as.data.frame(d2)
colnames(newDist)   <- c('dist1', 'dist2', 'dist3', 'dist4')
newDist$cluster     <- newClust
newDist$prob        <- chiSq
newDist$cprob       <- 1-chiSq

# stratified random sample by cluster
set.seed(1)                                 # just for demo
by_cluster <- newDist %>% group_by(cluster)
t          <- sample_n(by_cluster, 20)
#table(t$cluster)

t2         <- t[ sample(1:nrow(t)), ]        # shuffle the row order
(t3        <- as.data.frame(t2) )            # change type and setup for nicer print

