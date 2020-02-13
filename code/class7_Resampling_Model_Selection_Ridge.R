# class13_resampling_model_selection

#------------------------------------------------
### Setup
#------------------------------------------------
library(ISLR)
library(tidyverse)
set.seed(1861)
data(Auto)

Auto_sub <- Auto %>% select(-name)
head(Auto_sub)


#------------------------------------------------
# Leave one out cross-validation
### LOOCV 
#------------------------------------------------

# for loop of model
mods_LOOCV <- list()
preds_LOOCV <- NULL
for(i in 1:nrow(Auto)){
  mod = lm(mpg ~ .,
           data = Auto_sub %>% slice(-i))
  preds_LOOCV[i] <- predict(mod, newdata =
                              slice(Auto_sub,i))
  mods_LOOCV[[i]] <- mod
}

head(preds_LOOCV)

mod_insample <- lm(mpg ~ ., data = Auto_sub)

# compute RMSE LOOCV 
preds_DF <- data.frame(
  preds_LOOCV = preds_LOOCV,
  preds_insample = predict(mod_insample),
  true = Auto$mpg
)


#------------------------------------------------
### k-fold Cross validation
#------------------------------------------------
Auto_sub <- 
  mutate(Auto_sub,
         folds = createFolds(Auto_sub$mpg,
                             k = 10, list = FALSE)
  )

Auto_sub$folds

### K-Fold Cross Validation
nfolds <- 10
preds_10FoldCV_DF <- data.frame(
  folds = Auto_sub$folds,
  preds_10FoldCV = rep(NA,nrow(Auto_sub))
)

for(i in 1:nfolds){
  mod <- lm(mpg ~ ., 
            data = Auto_sub %>% 
              filter(folds != i))
  preds <- predict(mod, 
                   newdata = Auto_sub %>%
                     filter(folds == i))
  preds_10FoldCV_DF[preds_10FoldCV_DF$folds == i,"preds_10FoldCV"]  <- preds
}


preds_DF <- data.frame(
  preds_10FoldCV = preds_10FoldCV_DF$preds_10FoldCV,
  preds_DF  
)

RMSE(preds_DF$preds_10FoldCV,preds_DF$true)
RMSE(preds_DF$preds_LOOCV,preds_DF$true)
RMSE(preds_DF$preds_insample,preds_DF$true)

R2(preds_DF$preds_10FoldCV,preds_DF$true)
R2(preds_DF$preds_LOOCV,preds_DF$true)
R2(preds_DF$preds_insample,preds_DF$true)


#------------------------------------------------
### Bootstrapping
#------------------------------------------------
B = 100 # number of bootstraped datasets
n_boot = 200 # size of each bootstrapped sample
coef_boot = NULL
for(b in 1:B){
  idx <- sample(1:nrow(Auto_sub), 
                size = n_boot, replace = TRUE)
  mod <- lm(mpg ~ displacement, 
            data = Auto_sub %>% slice(idx))
  coef_boot[b] <- mod$coefficients[2]  
}

mod_lm <- lm(mpg ~ displacement, 
             data = Auto_sub)

coef_boot <- data.frame(coef_boot = 
                          coef_boot)

ggplot(coef_boot, aes(x = coef_boot)) + 
  geom_histogram() +
  geom_vline(xintercept = mod_lm$coefficients[2], 
             color = "red")


#------------------------------------------------
### Forward Stepwise Selection
#------------------------------------------------
# install.packages('leaps')
library('leaps')

auto_fit_fwd <- 
  regsubsets(mpg ~ . , 
             data = Auto_sub,
             nvmax = 7,
             method = "forward")

summary(auto_fit_fwd)

plot(auto_fit_fwd, scale = "adjr2")

#------------------------------------------------
### Backward Stepwise Selection
#------------------------------------------------
auto_fit_bkwd <- 
  regsubsets(mpg ~ . , 
             data = Auto_sub,
             nvmax = 7,
             method = "backward")
summary(auto_fit_bkwd)
plot(auto_fit_bkwd, scale = "adjr2")


#------------------------------------------------
### Ridge Regression
#------------------------------------------------
library(glmnet)
library(glmnetUtils)

# load the movies dataset
library('tidyverse')
options(scipen = 50)
set.seed(1861)
movies <- read.csv(here::here("datasets","movie_metadata.csv"))
movies <- movies %>% filter(budget < 400000000) %>% 
  filter(content_rating != "",
         content_rating != "Not Rated",
         !is.na(gross)) 
movies <- movies %>% 
  mutate(genre_main = unlist(map(strsplit(as.character(movies$genres),"\\|"),1)),
         grossM = gross / 1000000,
         budgetM = budget / 1000000,
         profitM = grossM - budgetM)
movies <- movies %>% mutate(genre_main = fct_lump(genre_main,5),
                            content_rating = fct_lump(content_rating,3),
                            country = fct_lump(country,2),
                            cast_total_facebook_likes000s = 
                              cast_total_facebook_likes / 1000,) %>%   drop_na()

train_idx <- sample(1:nrow(movies),size = floor(0.75*nrow(movies)))
movies_train <- movies %>% slice(train_idx)
movies_test <- movies %>% slice(-train_idx)

# estimate ridge mod 
Ridge_mod <- cv.glmnet(profitM ~ ., 
                       data = movies_train %>% 
                         select(-c(director_name,actor_1_name,
                                   actor_2_name,actor_3_name,
                                   plot_keywords,movie_imdb_link,
                                   country,budgetM,grossM, genres,
                                   language, movie_title)),
                       alpha = 0)

coef(Ridge_mod)

# explore how coefficients change as we change lambda
library(coefplot)
coefpath(Ridge_mod)
