# homework 11 functions !

##################################################
# function: reg_stats
# fits linear model, extracts statistics
# input: 2-column data frame (x and y)
# output: slope, p-value, and r2
#------------------------------------------------- 
reg_stats <- function(x){
. <- lm(data=x, dbh~ht)
sum <- summary(.)
stats_list <- list(slope = sum$coefficients[2,1], pVal = sum$coefficients[2,4], r2 = sum$r.squared)

return(stats_list)
}


##################################################
# function: graphing
# fits linear model
# input: 2-column data frame (x and y)
# output: linear graph
#------------------------------------------------- 
graphing <- function(x){
ht <- x[,2]
dbh <- x[,1]
lmdata <- lm(data=x, dbh~ht)
sum2 <- summary(lmdata)
ggplot(x, aes(ht, dbh)) +
  geom_point() +
  geom_abline(slope = sum2$coefficients[2,1], intercept = sum2$coefficients[1,1], aes(color = "red"))
}
