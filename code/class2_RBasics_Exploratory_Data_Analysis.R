# workflow

# coding basics
# mathematics
1 / 300
3 * 45
4^3
sin(3*pi)

# Variables
x <- 1

y <- 3

z <- x * y

print(z)

# functions
seq(1,10)

seq(1,12, by = 3)

# vectors
myVec <- c(5, 3, 7)

# characters
x <- "hey"
x

# character vectors
my_Char_Vec <- c("hey", "you", "guuuuyyys")
my_Char_Vec

# Factors
month_factor <- c("Dec", "Dec", "Jan", "Mar")
month_factor <- factor(month_factor)

str(month_factor)
nlevels(month_factor)
levels(month_factor)

sort(month_factor)

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

month_prop_factor <- factor(month_factor, levels = month_levels)

month_prop_factor

# functions
thank_you_next <- function(){
  print("Thank you")
}

thank_you_next()

### Data Transform


# Let's load some more fun data, a movies database
# Download from here https://www.kaggle.com/carolzhangdc/imdb-5000-movie-dataset/downloads/imdb-5000-movie-dataset.zip/1
# Or on Blackboard
# note we must use "/" -- not "\"! 
movies <- read.csv("datasets/movie_metadata.csv", 
  stringsAsFactors = FALSE)

# let's try a new package to see how this summarizes data
install.packages('skimr')
skimr::skim(movies)


# pipe operator
# The pipe operator "%>%" is super useful!
# It allows us to execute a series of functions on an object in stages
# The general recipe is Data_Frame %>% function1() %>% function2() etc
# Functions are applied right to left
library(tidyverse)
movies %>% summary() 
summary(movies)

# filter() to remove rows
# filter removes out certain rows for us
moviesEng <- movies %>% filter(language == "English")
dim(moviesEng)

# see unique values of a factor
unique(moviesEng$language)
is.character(moviesEng$language)
is.factor(moviesEng$language)
head(moviesEng)

moviesBig <- movies %>% filter(budget > 100000000)
head(moviesBig)

# logical operators and or function as well
moviesSub <- movies %>% filter(language == "English" | language == "Spanish")
dim(moviesSub)

moviesBig_Spanish <- movies %>% filter(language == "Spanish" | budget > 1e10)
dim(moviesBig_Spanish)
View(moviesBig_Spanish)

library('magrittr')
moviesBig_Spanish %<>% select(movie_title, director_name, gross, budget, everything())
head(moviesBig_Spanish)

# missing values
# missing values are stored as NAs
is.na(NA)
1 > NA
1 + 1 == NA
NA == NA
y <- NA
y
x <- 1
y == x

moviesBig_Spanish %<>% mutate(profit = gross - budget) %>% 
 select(movie_title, director_name, gross, budget, profit, everything())

# arrange to order the rows
moviesBig_Spanish <- moviesBig_Spanish %>% arrange(desc(budget))

# slice to see certain rows
moviesBig_Spanish %>% slice(1:10)


# arrange via multipe columns
moviesBig_Spanish <- moviesBig_Spanish %>% arrange(desc(budget), desc(title_year))

# selecting columns using the select() function
moviesKeys <- movies %>%  select(director_name, movie_title)
head(moviesKeys)

# using select to programmatically select several variables
moviesActors <- movies %>% select(starts_with("actor"))
head(moviesActors)

# everything() is a useful function
movies <- movies %>% select(director_name, movie_title, title_year, everything())
names(movies)

# use the rename function to rename variables
movies <- movies %>%  rename(director = director_name)
names(movies)

# adding new variables using mutate()
# note %<>% == DF <- DF %>%  
# let's transform budget to be in millions
movies %<>% mutate(budgetM = budget/1000000,
                   grossM = gross/1000000)

# we can also create the natural logarithm of budget
movies %<>% mutate(logBudget = log(budget)) 

# let's plot imdb_score against budget
ggplot(data = movies) + geom_point(mapping = aes(x = imdb_score, y = budgetM))

# so it looks like there's some outliers
# The most expensive movie ever made was Pirates of the Caribbean: On Stranger Tides
# which cost $387.8m. Any movies with a budget higher than this must be a data anomaly

# Let's use the filter command to remove these

movies <- movies %>% filter(budgetM < 400) 

# group summaries using summarise and group_by
Director_Avg <- movies %>% group_by(director) %>%
  summarize(grossAvgDir = mean(grossM, na.rm = TRUE))

# sort by largest budget
Director_Avg %<>% arrange(desc(grossAvgDir))

# slice to see rows
Director_Avg %>% slice(1:20)

# total gross by 
Director_Tot <- movies %>% group_by(director) %>%
  summarize(grossTotDir = sum(grossM, na.rm = TRUE))

Director_Tot %>% arrange(desc(grossTotDir)) %>%  slice(1:20)

# counts 
Director_Num <- movies %>% group_by(director) %>% 
  summarize(
    n = n()
  )

Director_Num %>% arrange(desc(n)) %>% slice(1:20)

# Tim Burton really made 16 films?
movies %>% filter(director == "Tim Burton")

# what are all the Spike Lee joints
movies %>% filter(director == "Spike Lee")


# other summary functions
Director_Stats <- movies %>% group_by(director) %>% 
  summarize(
    n = n(),
    min = min(grossM),
    max = max(grossM),
    avg = mean(grossM),
    sd = sd(grossM))

Director_Stats %>% arrange(desc(max))

# who makes the highest average profit?
