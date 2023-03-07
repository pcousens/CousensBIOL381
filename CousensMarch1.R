# Lab/Lecture
# 1 March 2023

library(tidyverse)
# reproducable and clean data sets

# dplyr focused on manipulating data sets

# template data set we are using: starwars
data("starwars")
class(starwars)
glimpse(starwars)

# first have to clean the dataset
starwarsClean <- starwars[complete.cases(starwars[,1:10]),]
  # complete.cases : only checks if they were complete within the first and tenth rows

# check if there is an NA at one particular datapoint
is.na(starwarsClean[1,1])

# check for NAs throughout the dataset
anyNA(starwarsClean)


# the core verbs:

# filter() : subset observations by their values
    # uses conditional statements and logical operators (>, <, !=, ==) and | .
filt <- filter(starwarsClean, gender=="feminine" & height<180)
  # will give you a table of all rows that fit those characteristics
filt <- filter(starwarsClean, gender=="feminine" & height<180, height>100)
  # %in% is used for a few conditions, similar to ==
filt <- filter(starwars, eye_color %in% c("blue", "brown"))

# arrange() : reorder rows by either ascending or descending -- whatever you want
  # ascending height
order <- arrange(starwarsClean, by=height)
  # descending height
order <- arrange(starwarsClean, by=desc(height))
order <- arrange(starwarsClean, height, desc(mass))

# select() : choosing variables/columns by their names
  # keep all columns, but only 1-10 rows
starwarsClean[1:10,] # Base r
select(starwarsClean, 1:10)
  # or you can just call the name of the column itself
x <- select(starwarsClean, name)
  # using "-" to subset everything from film column to starship column
x <- select(starwarsClean, -(films:starships))

# rearrange columns
  # everything() refers to everything else
x <- select(starwars, gender, name, species, everything())
  # all columns that contain the word "color"
x <- select(starwarsClean, contains("color"))

# rename columns
  # newname = oldname
x <- select(starwarsClean, haircolor=hair_color)
x <- rename(starwarsClean, haircolor=hair_color)

# summarize() and group_by() : collapses values down to single summary points
  # calculate the mean height and format it as a table value
x<- summarize(starwarsClean, meanHeight=mean(height))
  # won't work if there is still an NA in the dataset
  # calculate the mean height, negating all NAs and see the total number of data points used
summarize(starwars, meanHeight=mean(height, na.rm=TRUE), TotalNumber=n())

# use group_by to specify grouping variables
starwarsGender <- group_by(starwars, gender)
# now, it will first group genders, and then calculate the mean height of each gender
summarize(starwarsGender, meanHeight=mean(height, na.rm=TRUE), number=n())

# mutate() : adding/creating new variables
  # creating new variable from predetermined data -- will show up as the last column
x <- mutate(starwarsClean, ratio=height/mass)
  # converting kg to lbs
starwars_lbs <- mutate(starwarsClean, mass_lbs=mass*2.2)
  # make the mass_lb column the fourth colums
select(starwars_lbs, 1:3, mass_lbs, everything())
  # if we only want the new variable
transmute(starwarsClean, mass_lbs=mass*2.2)
transmute(starwarsClean, mass, mass_lbs=mass*2.2)

# Pipe statements, or piping %>%
  # pipe statements translate roughly to "and then...."

  # take Clean dataset, then group by gender, then calculate the means
starwarsClean2 <- starwarsClean %>%
  group_by(gender)%>%
  summarize(meanHeight=mean(height, na.rm=TRUE), TotalNumber=n())

starwarsClean2 <- starwarsClean%>%
  mutate(sp = case_when(species == "Human" ~ "Human", TRUE ~ "Non-Human")%>%
  select(name, sp, everything())
view(starwarsClean)

# pivoting our datasets
starwarsClean
  # pre-pivot
starwarsClean%>%
  select(name, sex, height)

# long format to wide format
  # saying that we want to seperate the sex column into two different things -- making it a wider format
wideSW <- starwarsClean%>%
  select(name, sex, height)%>%
  pivot_wider(names_from=sex, values_from=height, values_fill=NA)

# wide format to long format
  # specify columns that you want data to be changed into
x <- wideSW%>%
  pivot_longer(cols=male:female, names_to="sex", values_to="height",values_drop_na=TRUE)
view(x)

