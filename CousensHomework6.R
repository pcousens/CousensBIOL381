# Cousens Homework 6
# 2 March 2023

library(tidyverse)
data("iris")
class(iris)

# question 1
#### 150 obs. , 5 variables

# question 2

iris1 <- filter(iris, Species %in% c("virginica","versicolor") & Sepal.Length> 6 & Sepal.Width> 2.5)
## filter from dataset iris species names, then sepal length, then sepal width using logical fucntions
#### 56 obs. , 5 variables

# question 3

iris2 <- select(iris1, Species, Sepal.Length, Sepal.Width)
## select from dataset iris1 only species | length | width
#### 56 obs. , 3 variables

# question 4

iris3 <- arrange(iris2, desc(Sepal.Length))
## order in descending length from dataset iris2
print(iris3[1:6,])

# question 5

iris4 <- mutate(iris3, Sepal.Area=Sepal.Length*Sepal.Width)
## add column named Sepal.Area which contains the L*W
#### 56 obs. , 4 variables

# question 6

iris5 <- summarize(iris4, Avg.Length=mean(Sepal.Length, na.rm=T), Avg.Width=mean(Sepal.Width, na.rm=T), Sample.Size=n())
print(iris5)
### calculate the average length, average width, and sample size of the whole data set -- display in a table

# question 7

iris6 <- group_by(iris4, Species)%>%
  summarize(Avg.Length=mean(Sepal.Length, na.rm=T), Avg.Width=mean(Sepal.Width, na.rm=T), Sample.Size=n())
print(iris6)
### first group by the different species, then calculate the average length, average width, and sample size of each individual species

# question 8

irisFinal <- iris %>%
  filter(Species %in% c("virginica","versicolor") & Sepal.Length> 6 & Sepal.Width> 2.5) %>%
  select(Species, Sepal.Length, Sepal.Width) %>%
  arrange(desc(Sepal.Length)) %>%
  mutate(Sepal.Area=Sepal.Length*Sepal.Width) %>%
  group_by(Species) %>%
  summarize(Avg.Length=mean(Sepal.Length, na.rm=T), Avg.Width=mean(Sepal.Width, na.rm=T), Sample.Size=n())
print(irisFinal)  
### reused lines from previous problems, but no longer had so specify the data set being used because I had already done that initially
  
# question 9
glimpse(iris)

irisLonger <- iris %>%
  select(Species,Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
  pivot_longer(cols= Sepal.Length:Petal.Width,
               names_to = "Measure",
               values_to = "Value")
print(irisLonger)

  






