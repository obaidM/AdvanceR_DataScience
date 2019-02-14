###
#
# Parameter tuning example using "broom" package
#
#    testing a single parameter: number of clusters
#
###
#
# 3 scenarios:  
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
#
#  All 3 scenarios structured to provide identical results
#
###
#

# conditional install
if(!require(plyr)      )  { install.packages('plyr');      library(plyr)      }
if(!require(tidyverse) )  { install.packages('tidyverse'); library(tidyverse) }
if(!require(broom)     )  { install.packages('broom');     library(broom)     }
rm(list=ls(all=TRUE))
# define constants (make it easier to change scenarios)
seed        <- 2014 # RNG seed, for real use get one from random.org
minClusters <-    1 # minimum number of clusters in scenario
maxClusters <-   10 # maximum number of clusters in scenario
nTrials     <-    5 # number of initial attempts to find good starting point (param to kmeans() )

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
# note: 'inflate' is depricated, use 'tidyr::crossing' instead
d        <-  data.frame(a = 1:3, b = 8:10)
#t       <-  d  %>%
#            inflate(x = c("apple", "orange"), y = c("car", "boat"))
t        <-  d %>%
  crossing(x= c("apple", "orange"), y= c("car", "boat") )
#### Assign different combinations 

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
# note: this replicate the full data for each value of "k"
#
kclusts  <-     kdat                                   %>%  ### we have 300 rows
  crossing(k = minClusters:maxClusters)  %>%            ### now we will have 3000 rows because of crossing function
  group_by(k)                            %>%
  do(clust = kmeans(select(., x1, x2), .$k[1], nstart = nTrials))
if ( debugPrint ) kclusts

# extract the total.withinss values for evaluation ( not in article )
kc_sse   <-     rep(NA,maxClusters)  # allocate space
t        <-     kclusts$clust        # extract list of kmeans result objects

for ( i in minClusters:maxClusters ) {
  t2        <-  t[[i]]             # extract i-th object from list
  kc_sse[i] <-  t2$tot.withinss    # extract and store desired value from object
}

if( debugPrint ) kc_sse

####
# look at the result of the crossing() and group_by() 
#
#  kdat has 300 obs; kclusts has 3000
#
#  kclusts has 10 copies (replications) of kdat
#

if (debugPrint ) { 
  t        <-     kdat %>%
    crossing(k = 1:10) %>%
    group_by(k)
  
  t2       <-     object.size(kdat)  #   300 obs, 2 vars of analytical interest (x1, x2)
  print(t2, units="auto")
  t3       <-     object.size(t)     # 3,000 obs, 2 vars of analytical interest (x1, x2)
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

cross_sse    <- crossing(k = minClusters:maxClusters) %>%
  group_by(k)                           %>%
  do(data.frame(twss=kmeans(select(kd2, x1, x2), .$k[1], 
                            nstart = nTrials)$tot.withinss ))
if ( debugPrint ) cross_sse

###
#
#  scenario 3
#
#  non-tidy, non-broom approach
#     ( see demo_5.4b2_kmeans_boston_expansion )
#
#  minimizes memory use ( sometimes helpful ) only 1 copy of kdat
#
set.seed(seed)

sse <- rep(-1, maxClusters) # based on page 17 related code

for ( i in 1:10 ) {
  sse[i] <- kmeans(select(kd2,x1, x2), i, nstart = nTrials)$tot.withinss
}

if ( debugPrint ) sse

###
#
# compare results between kclusts and sse
#
###

all.equal(sse, kc_sse)             # TRUE if they match
all.equal(sse, cross_sse$twss)

