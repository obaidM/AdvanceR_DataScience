

rm(list = ls())

library(MASS)
library(broom)
library(tidyverse)
library(tolerance)

options(scipen=10)
data(Boston)  
df<- Boston[complete.cases(Boston), ]

set.seed(2018)

b <- 500 # number of bootstrap index sequences
t <- replicate(b,df[sample(nrow(df),replace=T),], simplify=F)
## simplify=F means that output will be a list and not a matrix or vector
## 
est.param <- data.frame()




for(i in 1:b)
{
  data.b <- t[[i]]
  model.b <- lm(medv~., data=data.b)
  temp <- tidy(summary(model.b))
  est.param <- rbind(est.param,temp)
}

############ balanced code 
##each data appears the same number of times in all the resamples.
set.seed(2018)
b <- 500 # number of bootstrap samples
n <- 506 # number of original observations
est.balanced <- data.frame()


i <- sample(rep(1:n, b))                                # replicate 1 to n b times. generate and shuffle, work this in parts if needed
g <- matrix(data=i, ncol=n, byrow=T)

# 
for (j in 1:nrow(g)) {
  k <- g[ j, ]
  data.b <- df[k,]
  model.b<- lm(medv~.,data= data.b)
  temp <- tidy( model.b)
  est.balanced <- rbind(est.balanced,temp)
}


#########################################
#### PART B 

####   EST.PARAM unbalanced 

## parametric --- unbalanced

df <- filter(est.param, term == "ptratio")
## deg of freedom will be 499 
q <- mean(df$estimate) + qt(0.975,499)*sd(df$estimate) ### 95% use 0.975 and for 90% use 0.95
print(q)

print(mean(df$estimate))
print(sd(df$estimate))
print(median(df$estimate))



#### Non parametric ---- unbalanced 
df <- filter(est.param,term == "nox")
( ci95 <- c(0.025, 0.975) )
quantile(df$estimate, probs=ci95) # 0.95 CI 

df <- filter(est.param,term == "lstat")
( ci90 <- c(0.05, 0.95) )
quantile(df$estimate, probs=ci90) # 0.90CI 



###########################################
##########################################
###########################################



####   EST.balanced 

## parametric --- balanced

df <- filter(est.balanced, term == "rm")
## deg of freedom will be 499 
q <- mean(df$estimate) + qt(0.975,499)*sd(df$estimate)
print(q)

print(mean(df$estimate))
print(sd(df$estimate))
print(median(df$estimate))



#### Non parametric ---- balanced
df <- filter(est.balanced,term == "rad")
( ci95 <- c(0.025, 0.975) )
quantile(df$estimate, probs=ci95) # 0.95 CI 

df <- filter(est.balanced,term == "rad")
( ci90 <- c(0.05, 0.95) )
quantile(df$estimate, probs=ci90) # 0.90CI 
