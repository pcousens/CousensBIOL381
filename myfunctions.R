# MY FUNCTIONS

# clean data function + ratio of XCorr to UniqXCorr
cleandata <- function(x){
  x[,3]<-as.numeric(x[,3])
  x$u <- x[,1]/x[,3]
  return(x)
}

# mean and sd calculator function 
meanf <- function(a, x){
  xmean <- mean(a$u[a$b == x], na.rm=TRUE)
  xsd <- sd(a$u[a$b == x], na.rm=TRUE)
  mandsd <- c(xmean, xsd)
}

# creating a fake data frame function
fake <- function(x,y,z){
  xcval <- data.frame("XCorr", rnorm(209, x[1], x[2]))
  colnames(xcval) <- c("CorrValue", "Ratio")
  dxcval <- data.frame("X..916.Corr", rnorm(2019, y[1], y[2]))
  colnames(dxcval) <- c("CorrValue", "Ratio")
  uxcval <- data.frame("Uniq....916.corr", rnorm(209, z[1],z[2]))
  colnames(uxcval) <- c("CorrValue", "Ratio")
  fset <- rbind(xcval, dxcval, uxcval)
}

# anova test function for fake data set
anov <- function(x){
  fakeanov <- aov(Ratio~CorrValue, data = x)
  print(summary(fakeanov))
  boxplot(data=x, Ratio~CorrValue)
  
}
