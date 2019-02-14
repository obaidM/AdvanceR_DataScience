
rm(list = ls())
library(MASS)
library(broom)
library(tidyverse)
library(boot)
data(Boston)  
df<- Boston[complete.cases(Boston), ]

set.seed(2018)


model <- lm(medv~., data=df)

res1.simple <- tidy(summary(model))

####### boot sample 500 times 
##boot(Portfolio ,alpha.fn,R=1000)
my.fn=function ()
{  
  return(model$residuals)
}


boot(df,my.fn,500)
