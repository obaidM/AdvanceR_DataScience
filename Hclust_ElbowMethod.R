
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
# code to fetch the packages

require(MASS     )
require(tidyverse)
require(broom    )

rm(list=ls(all=TRUE))

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
#   user choices
#
minClusters     <- 1     # minimum number of clusters (see code)
maxClusters     <- 25    # maximum number of clusters (see code)

# distance function equiv to ESS (see ISLR, section 10.3.1)
hc <- hclust(dist(myBoston)^2, method="complete")

# run from minClusters to maxClusters and calculate the WSS
hc.twss    <- rep(-1,maxClusters)

# calculate penalty function (within group error sum of squares) [ direct ESS ]
wssf       <- function(df) {
  t <- scale(df, center=T, scale=F) # center around the mean
  return( sum(t^2) )
}


tB                <- myBoston # local working copy

for (i in minClusters:maxClusters) {
  # retrieve the cluster memberships
  
  tB$cluster    <- cutree(hc, k=i)
  tB2           <- tB                               %>%
    group_by(cluster)                %>%
    do(data.frame(twss = wssf(.) ) )
  hc.twss[i]    <- sum(tB2$twss)
}


# plot results to find optimal tradeoff (num clusters v. total winthin-ss)
plot( minClusters:maxClusters,
      #hc.totwss,  ## this is an error 
      hc.twss,
      type="b",
      xlab="Number of Clusters (hclust)",
      ylab="Aggregate within cluster SSE (sum of squared error)")

# alternate representation: 1st order differences
n   <- length(hc.twss)
d   <- hc.twss[1:(n-1)] - hc.twss[2:n]
d1 <-  diff(hc.twss)
plot(minClusters:(maxClusters-1),
     d,
     type="b",
     xlab="Number of Clusters (hclust)",
     ylab="Total Within-Group SS (1st order differences)" )

###
# proportion of unexplained variability remaining
###
remain <- hc.twss / max(hc.twss)
remain
