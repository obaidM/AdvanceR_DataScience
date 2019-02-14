
#     k-means algorithm description/explanation:
#
#          user- supplied input parameters:
#
#                k:         number of clusters
#                nstart:    number of initializing trials
#                iter.max:  maximum number of iterations (repetitions)
#
#          (1) randomize observations into "k" initial groups (keep best of "nstart" trials)
#          (2) calculate centroid (vector of arithmetic means) for each cluster
#          (3) calculate within-sum-of-squares (ESS) for each cluster (retain value)
#          (4) re-assign observations into closest cluster 
#                 (distance from centroid; retain count of re-assignments)
#          (5) if iterations <= "iter.max" and count of re-assignments > 0, go to (2)
#          (6) finished
#
###
# code to fetch the package if it is not present
rm(list=ls(all=TRUE))

if ( !require(MASS     ) ) { install.packages('MASS');      library(MASS)      }
if ( !require(tidyverse) ) { install.packages('tidyverse'); library(tidyverse) }
if ( !require(broom    ) ) { install.packages('broom');     library(broom)     }

data(Boston)                                 # from MASS

###
#
# filter for complete cases (no missing data)
#
###
myBoston        <- Boston[ complete.cases(Boston), ]


dim(Boston)
dim(myBoston)
###
#
# kmeans: fixed number of clusters
#
#   cluster data based on error-sum-of-squares
#   random starting points
#   user specified number of clusters
#   user specified number of starting trials (best one is automatically selected)
#   user specified maximum number of iterations
#
###
#   user choices
#
seed            <- 1     # random number generator seed
minClusters     <- 1     # minimum number of clusters (see code)
maxClusters     <- 20    # maximum number of clusters (see code)
km.nstart       <- 10    # number of starting trials
km.iter.max     <- 20    # iteration limit

###
# start skip of code
###

# track the behavior (initialize to an illegal value) [ total within-ss ] (elbow method)
#sse             <- rep(-1, maxClusters)

# test results for each clustering scenario
#for (i in minClusters:maxClusters) {
#    set.seed(seed)                          # reset RNG each time
#    sse[i]      <- kmeans(myBoston,
#                          centers=i,
#                          nstart=km.nstart,
#                          iter.max=km.iter.max)$tot.withinss
#    }

# plot results to find optimal tradeoff (num clusters v. total winthin-ss)
#plot( minClusters:maxClusters,
#      sse,
#      type="b",
#      xlab="Number of Clusters",
#      ylab="Aggregate within cluster SSE (sum of squared error)")

# zoom in to identify the choice more easily
#clust1  <- 3
#clust2  <- 8
#sse2    <- sse[ clust1:clust2 ]
#plot( clust1:clust2,
#      sse2,
#      type="b",
#      xlab="Number of Clusters",
#      ylab="Aggregate within cluster SSE (sum of squared error)")

###
# skip to here
###

# test case: 3 clusters
set.seed(seed)
t      <- kmeans(myBoston,
                 centers=3,
                 nstart=km.nstart,
                 iter.max=km.iter.max)

# measures of interest
table(t$cluster)
t$tot.withinss   # overall
t$withinss       # by cluster

# reconstruct the error measurements
t2         <- myBoston
t2$cluster <- t$cluster   # add cluster number for each row (observation) ## WHICH CLUSTER they belong to

# calculate penalty function (within group error sum of squares)
wssf       <- function(df) {
  t <- scale(df, center=T, scale=F) # center around the mean
  return( sum(t^2) )
}

# by cluster
t3         <- t2 %>%
  group_by(cluster) %>%
  do( data.frame(wss = wssf(.) ) )

sum(t3)  # overall
t3       # by cluster

# individual divergences
t$withinss - t3$wss

# overall divergence
t$tot.withinss - sum(t3$wss)

###
#
# k-means WSS clculations woking correctly
#
# now, apply it to hclust() to select a number of clusters
#
# use: squared euclidean distance as metric for clustering
#      method="complete"
#
###
