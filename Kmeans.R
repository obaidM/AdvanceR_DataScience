###
#
# Introduction to Clustering demo
#
#     k-means clustering
#
# History:
#
#     2018/07/11   Initial code (copied from other example documents)     walter johnston
#
###
#
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

if ( !require(MASS) ) {
  install.packages('MASS'); library(MASS) }

data(Boston)                                 # from MASS

###
# standardize the variables to eliminate scale effects
#
# filter for complete cases (no missing data)
#
# transform each variable into a Z-score via scale()
###
myBoston        <- Boston[ complete.cases(Boston), ]

#myBoston        <- scale(myBoston) 

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

# track the behavior (initialize to an illegal value) [ total within-ss ] (elbow method)
sse             <- rep(-1, maxClusters)

# test results for each clustering scenario
for (i in minClusters:maxClusters) {
  set.seed(seed)                          # reset RNG each time
  sse[i]      <- kmeans(myBoston,
                        centers=i,
                        nstart=km.nstart,
                        iter.max=km.iter.max)$tot.withinss
}

# plot results to find optimal tradeoff (num clusters v. total winthin-ss)
plot( minClusters:maxClusters,
      sse,
      type="b",
      xlab="Number of Clusters",
      ylab="Aggregate within cluster SSE (sum of squared error)")