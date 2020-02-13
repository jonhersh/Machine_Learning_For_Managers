### setup of data
movies <- read.csv("datasets/movie_metadata.csv")

# Simple method of testing/training split
set.seed(234)  # any seed will do
n_obs <- nrow(movies)
movies %<>% mutate(grossM = gross / 1e6,
                   budgetM = budget / 1e6,
                   profitM = grossM - budgetM) %>% 
  filter(budgetM < 400)

# set training data to 75%
train_idx <- sample(n_obs*0.75) 

# grab rows matching train index for training
movies_train <- movies %>% slice(train_idx)
head(movies_train)


### log transform X variable
movies_train %<>% mutate(logbudgetM = log(budgetM)) 

mod5 <- lm(grossM ~ budgetM + imdb_score, data = movies_train)
summary(mod5)

mod6 <- lm(grossM ~ logbudgetM + imdb_score, data = movies_train)
summary(mod6)

ggplot(movies_train, aes(x  = budgetM)) + geom_histogram() +
  labs(x = "Budget ($M USD)")

ggplot(movies_train, aes(x  = logbudgetM)) + geom_histogram() +
  labs(x = "Log Budget ($M USD)")

### Log Transforming Y variable 
mod7 <- lm(log(grossM) ~ logbudgetM + imdb_score, data = movies_train)
summary(mod7)

movies_train %<>% mutate(imdb_sq = imdb_score * imdb_score) 

mod8 <- lm(grossM ~ logbudgetM + imdb_score + I(imdb_score^2), data = movies_train)
summary(mod8)


### 
data(Auto)
# Plot linear relationship 
ggplot(Auto, aes(x = horsepower, y = mpg)) + geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "y = horsepower")

data(Auto)
# plot nonlinear relationship
ggplot(Auto, aes(x = horsepower, y = mpg)) + geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  labs(title = "y = horsepower + horsepower^2")

# estimate model
mod1 <- lm(mpg ~ horsepower + I(horsepower^2), data = Auto)
summary(mod1)$coefficients 

# calculate mpg prediction by levels of horsepower by hand
mod1$coefficients[1] + mod1$coefficients[2]*50 + mod1$coefficients[3]*50^2
mod1$coefficients[1] + mod1$coefficients[2]*60 + mod1$coefficients[3]*60^2

# use the margins command
library(margins)
m <- margins(mod1, at = list(horsepower = seq(50,200, by = 10)))
             
# plot using the margins command
cplot(mod1, x = "horsepower", what = "prediction",scatter = TRUE, 
      xlab = "Horsepower",ylab = "mpg")
print(m)

### factor levels 
table(fct_lump(movies_train$director_name, n = 2))
movies_train %<>% filter(!director_name == "")
movies_train %<>% filter(!director_name == "Woody Allen")
table(fct_lump(movies_train$director_name, n = 2))

movies_train <- movies_train %>% mutate(director_easy = fct_lump(director_name, n= 3))


# model with two factor levels
mod3 <- lm(grossM ~ budgetM + imdb_score + director_easy, 
           data = movies_train)
summary(mod3)

# relevel base
mod3 <- lm(grossM ~ budgetM + imdb_score + relevel(director_easy, ref = "Other"), 
           data = movies_train)
summary(mod3)


# marginal effects of imdb score
install.packages('margins')
library('margins')
m <- margins::margins(mod8, at = list(imdb_score = c(1:10)))
m
margins::cplot(mod8, x = "imdb_score", what = "prediction",
               scatter = TRUE)


plot(mod8)




### generating predictions
preds <- predict(mod1)

# put model preds and true in a data frame
mod1_df <- data.frame(
  preds = preds,
  true = Auto$mpg
)

### predicted true plots
ggplot(mod1_df, aes(x = true, y = preds)) + geom_point() + 
  geom_abline(color = "red", linetype = "dashed") + 
  xlim(10,40) + ylim(10,40)

### model residuals
mod1_df <- data.frame(
  preds = preds,
  true = Auto$mpg,
  resids = mod1$residuals
)

# plot residuals against predicted values
ggplot(mod1_df, aes(x = preds, y = resids)) + geom_point()

# calculate summary of residuals
sum(mod1$residuals)

data(Auto)

# examine max and min residuals
Auto_sub <- data.frame(Auto, resids = mod1$residuals) %>% 
  arrange(desc(resids))
Auto_sub %>% slice(1)
Auto_sub %>% slice(392)

# calculate leverage or hat values
Auto_sub <- data.frame(Auto, hats = hatvalues(mod1))
Auto_sub %>% arrange(desc(hats)) %>% slice(1:2)

plot(mod1)

# calculate VIF of predictors 
library('olsrr')
ols_vif_tol(mod1)
