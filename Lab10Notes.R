# for loops and randomized tests
# lab 4-5-2023 notes

# basic anatomy of a function
#   name(~input~){~body~}
#           body: parameters that we want to give the function
#   ex: function(x,y,z){ x 
#                        y 
#                        z }


# randomization tests (needed for q.4 in homework)
#   H0: There is no difference in...
#   H1: There is a difference in...

# in an experiment where we dont know if out treatment has and actual
# effect, we shuffle everything around to randomize order
# and establish chance

# code from randomization test lecture
##################################################
# function: shuffle_data
# randomize data for regression analysis
# input: 2-column data frame (x_var,y_var)
# output: 2-column data frame (x_var,y_var)
#------------------------------------------------- 
shuffle_data <- function(z=NULL) {
  if(is.null(z)){
    x_obs <- 1:20
    y_obs <- x_obs + 3*rnorm(20)
    z <- data.frame(x_obs,y_obs)} # set up data frame                 
  z[,2] <- sample(z[,2]) # use sample function with defaults to reshuffle column
  
  return(z)
}