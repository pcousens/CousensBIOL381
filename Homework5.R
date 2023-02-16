## Homework5

### 1

n_dims <- runif(1, min=3, max=10)
a <- c(1:n_dims^2)
print(a)

b <- sample(x=a)
print(b)

c <- matrix(data=b, nrow= n_dims)
print(c)

d <- Conj(t(c))
print(d)

first <- sum(d[1,])
print(first)
firstm <- mean(d[1,])
print(firstm)

last <- sum(d[10,])
print(last)
lastm <- mean(d[10,])
print(lastm)

e <- sample(3:10, 1)
f <- eigen(e)
##### output are square numbers

typeof(e)


### 2

g <- runif(16)
my_matrix <- matrix(data=g, nrow=4)
print(my_matrix)

h <- sample(c(TRUE, FALSE), 100, rep=TRUE)
print(h)
i <- sample(c(0,1), 100, rep= TRUE)
my_logical <- i<1
print(my_logical)

my_letters <- sample(letters, 26)
print(my_letters)

j <- list(my_matrix[2,2],my_logical[[2]],my_letters[[2]])
print(j)

typeof(j)

k <- unlist(j)
print(k)
l <- c(k)
print(l)

typeof(l)

my_units <- runif(26, min=0, max=10)

my_letters <- sample(letters, 26)

m <- data.frame(my_units,my_letters)
print(m)

# did not work ...
n <- sample(my_units, 4)
print(n)
o <- replace(n,1:4,NA)
print(o)

p <- data.frame(my_units,(replace((sample(my_units, 22)),26,NA)),my_letters)
print(p)

q <- data.frame(my_units,(replace(sample(my_units,26),4,NA)),my_letters)
print(q)

# actual answer
m[sample(1:26, 4),1] <- NA

s <- which(is.na(m))
print(s)

m[,2] <- sort(m$my_letters)

t <- mean(m[,1])
print(t)
