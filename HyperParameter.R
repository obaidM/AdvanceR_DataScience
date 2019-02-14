###
#
# Parameter tuning example using "broom" package
#
#    testing a multiple parameters: number of clusters and algorithm in kmeans()
#
###
#
# 4 scenarios:  
#
#   (1) tidyverse, broom; full crossing w/ data and group_by (max memory);
#                         stores all kmeans() result objects for later use
#   (2) tidyverse, broom; full crossing w/o data and group_by;
#                         stores only penalty value (tot.withinss);
#                         kmeans() objects not recoverable [ can be recomputed ]
#   (3) base R; explicit enumeration of parameter values under consideration;
#                         stores only penalty value (tot.withinss);
#                         kmeans() result objects not recoverable [ can be recomputed ];
#                         code easily modified to make any single result recoverable
#    (4) tidyverse, broom, base R; generate full crossing data as in (2) but then
#                         loop manually across combinations of crossing parameters;
#                         simpler code, results easily merged crossing data, more
#                         explicit control
#
#  Scenarios NOT structured to provide identical results
#
###
#

# conditional install
if(!require(plyr)      )  { install.packages('plyr');      library(plyr)      }
if(!require(tidyverse) )  { install.packages('tidyverse'); library(tidyverse) }
if(!require(broom)     )  { install.packages('broom');     library(broom)     }

# define constants (make it easier to change scenarios)
seed        <- 2014 # RNG seed, for real use get one from random.org
minClusters <-    1 # minimum number of clusters in scenario
maxClusters <-   10 # maximum number of clusters in scenario
nTrials     <-    5 # number of initial attempts to find good starting point (param to kmeans() )
maxIter     <-   20 # set maximum number of iterations in the kmeans()
cMethod     <- c("Hartigan-Wong", "Lloyd", "MacQueen") # note: Lloyd and Forgy are same (see doc)

# define parameter for supplemental listings ( TRUE is listings on, FALSE is listings off )
debugPrint  <- TRUE

###
# page 16
#
# generate data for use with kmeans()
#

set.seed(seed)

# define characteristics of data to be analyzed
centers  <-  data.frame(  oracle = factor(1:3),
                          size = c(100, 150, 50),
                          x1 = c(5,0,-3),
                          x2 = c(-1, 1, -2))

# generate data for analysis
kdat  <-  centers           %>%
  group_by(oracle)  %>%
  do(data.frame( x1 = rnorm(.$size[1], .$x1[1]),
                 x2 = rnorm(.$size[1], .$x2[1])))

###
# page 17
#
# demo of what crossing() does
#
# note: 'inflate' is depricated, use 'tidyr::crossing' instead
d        <-  data.frame(a = 1:3, b = 8:10)
#t       <-  d  %>%
#            inflate(x = c("apple", "orange"), y = c("car", "boat"))
t        <-  d %>%
  crossing(x= c("apple", "orange"), y= c("car", "boat") )

if (debugPrint) t

# for reproducibility, reset the seed
set.seed(seed)

###
#
# scenario 1
#
# tidy, broom approach
#
# collect the kmeans() result objects in a dataframe for later use
#
# note: this replicates the full data for each value of "k"
#
kclusts  <-     kdat                                                     %>%
  crossing(k = minClusters:maxClusters, method = cMethod)  %>%
  group_by(k, method)                                      %>%
  do(clust = kmeans(select(., x1, x2), 
                    .$k[1], 
                    iter.max = maxIter,
                    algorithm= .$method[1], 
                    nstart = nTrials))
if ( debugPrint ) kclusts

# extract the total.withinss values for evaluation ( not in article )

t        <-     kclusts$clust        # extract list of kmeans result objects
kc_sse   <-     rep(NA,length(t))    # allocate space

for ( i in 1:length(t) ) {
  t2        <-  t[[i]]             # extract i-th object from list
  kc_sse[i] <-  t2$tot.withinss    # extract and store desired value from object
}

kclusts$twss  <-  kc_sse

if( debugPrint ) kc_sse

####
# look at the result of the crossing() and group_by() 
#
#  kdat has 300 obs; kclusts has 9,000 obs
#
#  kclusts has 30 copies (replications) of kdat
#

if (debugPrint ) { 
  t        <-     kdat                                                     %>%
    crossing(k = minClusters:maxClusters, method = cMethod)  %>%
    group_by(k, method)
  
  t2       <-     object.size(kdat)  #   300 obs, 2 vars of analytical interest (x1, x2)
  print(t2, units="auto")
  t3       <-     object.size(t)     # 9,000 obs, 2 vars of analytical interest (x1, x2)
  print(t3, units="auto")
}

###
#
#  scenario 2
#
#  tidy, broom approach with single (external) copy of data
#
#  note: store the tot.withinss for each model rather than the model itself [ discuss ]
#
set.seed(seed)

kd2 <- ungroup(kdat)

cross_sse    <- crossing(k = minClusters:maxClusters, method = cMethod) %>%
  group_by(k, method)                                     %>%
  do(data.frame(twss=kmeans(select(kd2, x1, x2), 
                            .$k[1],
                            iter.max = maxIter, 
                            algorithm= .$method[1],
                            nstart = nTrials)$tot.withinss ))
if ( debugPrint ) cross_sse

###
#
# scenario 3
#
# non-tidy, non-broom approach, base R, loop over each crossing dimension
#     ( see demo_5.4b2_kmeans_boston_expansion )
#
# minimizes memory use ( sometimes helpful ) only 1 copy of kdat
#
# Note: RNG could be reset inside inner loop to make kmeans() objects
#       easily recoverable
#
set.seed(seed)

sse <- rep(-1, length(kclusts$clust) ) # based on page 17 related code

k   <- 0                    # initialize sequence count
for ( i in 1:10 ) {         # loop on number of clusters ( outer loop )
  for ( j in cMethod ) {  # loop on algorithm choice   ( inner loop )
    k <- k+1
    sse[k] <- kmeans(select(kd2,x1, x2), 
                     i, 
                     iter.max = maxIter, 
                     algorithm= j,
                     nstart = nTrials)$tot.withinss
  } # for: j 
}     # for: i

if ( debugPrint ) sse

###
#
# scenario 4
#
# tidy, broom, base R, single loop ( simpler code )
#
# minimizes memory use ( sometimes helpful ) only 1 copy of kdat
#
# Note: RNG could be reset inside inner loop to make kmeans() objects
#       easily recoverable
#
set.seed(seed)

# cartesian product of elements
cross4  <- crossing(k = minClusters:maxClusters, method = cMethod)

n4      <- nrow(cross4)  # how many total entries

sse4    <- rep(-1, n4 )  # result storage

k4      <- cross4$k      # values for each component (expanded)
method4 <- cross4$method

# for easy recapture of result, insert set.seed() as 1st line inside loop
#
for ( i in 1:n4 ) {   
  sse4[i] <- kmeans(select(kd2, x1, x2),
                    k4[i],
                    iter.max  = maxIter,
                    algorithm = method4[i],
                    nstart    = nTrials)$tot.withinss
}

# label results
result4 <- data.frame( cross4, sse4 )
