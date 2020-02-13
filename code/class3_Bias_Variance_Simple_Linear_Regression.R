# Testing/Training Split
library('magrittr')
sni

# what happens when we want to add director?
mod3 <- lm(grossM ~ budgetM + imdb_score + director_name, 
           data = movies_train)

summary(mod3)

# too many director factor levels! What to do?
movies %>% count(director_name) %>% top_n(30) %>% slice(1:30)

# what actually is happening here? 
DF <- data.frame(y = rnorm(5),
                 x1 = 1:5,
                 x2 = c("A","B","B","A","C"))
model.matrix(y ~ x1 + x2, DF) 


# just take top 20 factor levels, rest are given "other" category
table(fct_lump(movies$director_name, n = 3))


table(fct_lump(movies$director_name, n = 20))

# simple_levels <- c("Michael Bay", "Steven Spielberg")


movies_train <- movies_train %>% mutate(director_factor = fct_lump(director_name, n = 20))


mod3 <- lm(grossM ~ budgetM + imdb_score + director_factor, data = movies_train)
summary(mod3)

movies %>%fct_recode(director_name, 
       levels = c("Steven Spielberg","Michael Bay"))

# coefplot
library(coefplot)
coefplot(mod3, sort = "magnitude") + 
  labs(title = "Regression Summary: Michael Bay Kicks A$$",
       subtitle = "Dependent variable: Movie Gross in $M",
       x = "Coefficient Magnitude",
       y = "Coefficient",
       caption = "Source: Top 5000 IMDB Movie Database")

ggsave(here::here("figures","Coefplot_IMDB_Budget_Director.pdf"), 
       width = 7, height = 8)


coefpath(mod3)
