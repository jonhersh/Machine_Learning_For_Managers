#------------------------------------------------------------
# Load data
#------------------------------------------------------------

library(partykit)
library(tidyverse)
library(titanic)


#------------------------------------------------------------
# Regression Trees
#------------------------------------------------------------
help(titanic)
summary(titanic_train)
library(PerformanceAnalytics)
chart.Correlation(titanic_train %>% select(-Name) %>% 
                    select_if(is.numeric), 
                  pch = 20,
                  histogram = TRUE)


head(titanic_train)

# clean data
titanic_df <- titanic_train %>% as_tibble() %>% 
  mutate(Suvived = if_else(Survived == 1, 
                           "Survived", "Dead"),
         Survived = as.factor(Survived),
         Sex = as.factor(Sex),
         Pclass = as.factor(Pclass)) %>% 
         mutate_if(is.character, as.factor)



# ctree to estimate model 
titanic_tree <- ctree(Survived ~ Sex + Pclass, 
                      data = titanic_df)
titanic_tree

plot(titanic_tree)


# estimate bigger model
titanic_tree_mod2 <- ctree(Survived ~ Sex + Pclass +
                               Age + SibSp + Fare,
                           data = titanic_df)
titanic_tree_mod2

plot(titanic_tree_mod2)

# cross validate to get optimal tree depth
# must use rpart package here
library(rpart)       
library(rpart.plot)  

titanic_mod_rpart <- rpart(Survived ~ Sex + Pclass +
                             Age + SibSp + Fare +
                             Ticket + Cabin + Embarked +
                             Parch,
                           data = titanic_df,
                           method = "anova",
                           control = list(cp = 0, 
                                          minsplit = 10,
                                          maxdepth = 5))


titanic_mod_rpart$cptable
plotcp(titanic_mod_rpart)


