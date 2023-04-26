## My functions

#######################################
# function: cleandata
# purpose: clean raw data
# input: raw data
# output: clean data, ratio of PPM to XCorr values 
#-------------------------------------- 
cleandata <- function(x){
  x[,2] <- as.numeric(x[,2])
  x$ratio1 <- x[,1]/x[,2]
  x$ratio2 <- x[,1]/x[,3]
  x$ratio3 <- x[,1]/x[,4]
  return(x)
}

#######################################
# function: meanf
# purpose: find mean of each ratio ppm:xcorr
# input: clean data
# output: list of means
#-------------------------------------- 
meanf <- function(data){
  xcorr <- filter(data)
  xm <- mean(xcorr$ratio1)
  dxcorr <- filter(data)
  dxm <- mean(dxcorr$ratio2) 
  uxcorr <- filter(data)
  uxm <- mean(uxcorr$ratio3)
means <- list(xm, dxm, uxm)
return(means)
}

#######################################
# function: sdf
# purpose: find sd of each ratio ppm:xcorr
# input: clean data
# output: list of sds
#-------------------------------------- 
sdf <- function(data){
  xcorr <- filter(data)
  xsd <- sd(xcorr$ratio1)
  dxcorr <- filter(data)
  dxsd <- sd(dxcorr$ratio2) 
  uxcorr <- filter(data)
  uxsd <- sd(uxcorr$ratio3)
  sds <- list(xsd, dxsd, uxsd)
  return(sds)
}

#######################################
# function: fake
# purpose: create fake data set modeled after real data
# input: real data set
# output: fake data set
#-------------------------------------- 
fake <- function(data) {
  ndxc <- rnorm(209, mean=means[[1]], sd=sds[[1]])
  nduxc <- rnorm(209, mean=means[[3]], sd=sds[[3]])
  fdf <- data.frame(xcorr= ndxc, uxcorr= nduxc)
  return(fdf)
}

#######################################
# function: piv
# purpose: make dataframe longer so all xcorrs are under one label (corrval)  
# input: wide dataframe
# output: long dataframe
#-------------------------------------- 
piv <- function(data) {
  plong <- pivot_longer(fakeset, cols=1:2, names_to = "corrval")
  print(plong)
  plong$corrval <- as.factor(plong$corrval)
}

#######################################
# function: an
# purpose: preform anova test on fake data
# input: long fake data set
# output: sum of squares, degrees of freedom, standard error
  # accept null hypothesis: there is no statistical difference between xcorr groups
#-------------------------------------- 
an <- function(data) {
  plong <- pivot_longer(fakeset, cols=1:2, names_to = "corrval")
  ano <- aov(plong$value~plong$corrval)
  return(ano)
}

#######################################
# function: box
# purpose: create box plot from data
# input: fake data
# output: box plot of xcorr and dxcorr versus value
#-------------------------------------- 
box <- function(data){
  plong <- pivot_longer(fakeset, cols=1:2, names_to = "corrval")
  bp <- boxplot(formula = plong$value~plong$corrval)
  print(bp)
}

#######################################
# function: meanf2
# purpose: find mean of each ratio, add 1 to the means of xcorr
# input: clean data
# output: list of means
#-------------------------------------- 
meanf2 <- function(data){
  xcorr2 <- filter(data)
  xm2 <- mean(xcorr2$ratio1) + 1
  dxcorr2 <- filter(data)
  dxm2 <- mean(dxcorr2$ratio2) 
  uxcorr2 <- filter(data)
  uxm2 <- mean(uxcorr2$ratio3)
  means2 <- list(xm2, dxm2, uxm2)
  return(means2)
}
# used the same sds from the original data analysis

#######################################
# function: fake2
# purpose: create fake data set modeled after real data, and increase sample size to 800
# input: real data set
# output: fake data set
#-------------------------------------- 
fake2 <- function(data) {
  ndxc2 <- rnorm(800, mean=means2[[1]], sd=sds[[1]])
  nduxc2 <- rnorm(800, mean=means2[[3]], sd=sds[[3]])
  fdf2 <- data.frame(xcorr2= ndxc2, uxcorr2= nduxc2)
  return(fdf2)
}

#######################################
# function: piv2
# purpose: make dataframe longer so all xcorrs are under one label (corrval)  
# input: wide dataframe
# output: long dataframe
#-------------------------------------- 
piv2 <- function(data) {
  plong2 <- pivot_longer(fakeset2, cols=1:2, names_to = "corrval")
  print(plong2)
  plong2$corrval <- as.factor(plong2$corrval)
}

#######################################
# function: an2
# purpose: preform anova test on fake ratios to determine significance
# input: long fake data set
# output: sum of squares, degrees of freedom, standard error
# accept null hypothesis: there is still no statistical difference between ratio groups
#-------------------------------------- 
anov2 <- function(data) {
  plong2 <- pivot_longer(fakeset2, cols=1:2, names_to = "corrval")
  ano2 <- aov(plong2$value~plong2$corrval)
  return(ano2)
}

#######################################
# function: boxp2
# purpose: create box plot from fake data
# input: fake data
# output: box plot of uxcorr2 and xcorr2 versus value
#-------------------------------------- 
boxp2 <- function(data){
  plong2 <- pivot_longer(fakeset2, cols=1:2, names_to = "corrval")
  bp2 <- boxplot(formula = plong2$value~plong2$corrval)
  print(bp2)
}
