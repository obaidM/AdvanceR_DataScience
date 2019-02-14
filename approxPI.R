
## Drop everything
rm(list=ls(all=TRUE))


# parameters

simulations <- 100000 # number of simulations
perimeter <- 1

slowtime <- system.time({
# randomly generate a point and check if it is in circle
f_point_in_circle <- function(perimeter=1){
  x1 <- runif(n=1, min=-perimeter, max=perimeter)  ## random uniform (n number between min & max)
  y1 <- runif(n=1, min=-perimeter, max=perimeter)
  return(list(x1=x1, 
              y1=y1,
              in_circle1=x1^2 + y1^2 <= perimeter^2))
}

# Monte Carlo simulations
set.seed(123)
pi_df <- data.frame(x1=rep(NA, simulations),  ## rep is for repeat
                    y1=rep(NA, simulations),
                    in_circle1=rep(NA, simulations))
    for (i in seq(simulations)){
  my_simulation <- f_point_in_circle()
  pi_df$in_circle1[i] <- my_simulation$in_circle1
  pi_df$x1[i] <- my_simulation$x1
  pi_df$y1[i] <- my_simulation$y1
}  

x1 <- pi_df$x1
y1 <- pi_df$y1
in_circle1 <- pi_df$in_circle1
my_pi1 <- (4 * sum(pi_df$in_circle1) / nrow(pi_df)) 

})

print(slowtime)
#############################
##### OPTIMIZED CODE
############################

fasttime<- system.time({
set.seed(123)
#simulations <- 10000 # number of simulations
#perimeter <- 1
t = runif(n=simulations*2,min=-perimeter, max=perimeter)
t = matrix(t,ncol = 2,byrow = TRUE)
x2 = t[ ,1]
y2 = t[,2]
in_circle2 <- (  (x2^2 + y2^2) <= perimeter^2)
pi_df2 <- data.frame(x2,y2,in_circle2)
my_pi2 <- (4 * sum(pi_df2$in_circle2)) / nrow(pi_df2) 

})

print(fasttime)

