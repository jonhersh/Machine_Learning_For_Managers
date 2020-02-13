# class8_Lasso_ElasticNet

#------------------------------------------------
### Lasso Regression
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
                              cast_total_facebook_likes / 1000) %>%   
  drop_na() %>% select(-c(director_name,actor_1_name,
                         actor_2_name,actor_3_name,
                         plot_keywords,movie_imdb_link,
                         country,budgetM,grossM, genres,
                         language, movie_title, budget, gross))

train_idx <- sample(1:nrow(movies),size = floor(0.75*nrow(movies)))
movies_train <- movies %>% slice(train_idx)
movies_test <- movies %>% slice(-train_idx)


# estimate Lasso mod 
Lasso_mod <- 
  
  cv.glmnet(profitM ~ ., 
            data = movies_train,
            alpha = 1)

coef(Lasso_mod, 
     s = Lasso_mod$lambda.min)

coef(Lasso_mod, 
     s = Lasso_mod$lambda.1se)

# put in a matrix
coef_mat <- data.frame(
  varname = rownames(coef(Lasso_mod)) %>% 
    data.frame(),
  Lasso_min = as.matrix(coef(Lasso_mod, 
                             s = Lasso_mod$lambda.min)) %>% 
    round(3),
  Lasso_1se = as.matrix(coef(Lasso_mod, 
                             s = Lasso_mod$lambda.1se)) %>% 
    round(3)
) %>% rename(varname = 1, 
             Lasso_min = 2, 
             Lasso_1se = 3)  %>% 
  remove_rownames()

coef_mat

plot(Lasso_mod)

# place in one coef


# explore how coefficients 
# change as we change lambda
library(coefplot)
coefpath(Lasso_mod)

#------------------------------------------------
### ElasticNet
#------------------------------------------------
alpha_list <- seq(0,1,len = 5)
alpha_list

enet_fit <- cva.glmnet(profitM ~ . ,
                       data = movies_train,
                       alpha = alpha_list)

enet_fit

minlossplot(enet_fit)

plot(enet_fit)

# look at each individual model
plot(enet_fit$modlist[[4]])

# view coefficients
coef(enet_fit, alpha = 0.75) %>% 
  round(3)

# if we want the lambda.min version
coef(enet_fit, alpha = 0.75, 
     s = enet_fit$modlist[[4]]$lambda.min)

coef(enet_fit, alpha = 1) %>% 
  round(3)

enet_coefs <- data.frame(
  varname = rownames(coef(enet_fit,alpha = 0)),
  ridge = as.matrix(coef(enet_fit, alpha = 0)) %>% round(3),
  alpha025 = as.matrix(coef(enet_fit, alpha = 0.25)) %>% round(3),
  alpha05 = as.matrix(coef(enet_fit, alpha = 0.5)) %>% round(3),
  alpha075 = as.matrix(coef(enet_fit, alpha = 0.75)) %>% round(3),
  lasso = as.matrix(coef(enet_fit, alpha = 1)) %>% round(3)
) %>% rename(varname = 1, ridge = 2, alpha025 = 3, alpha05 = 4, alpha075 = 5, lasso = 6) %>% 
  remove_rownames()

