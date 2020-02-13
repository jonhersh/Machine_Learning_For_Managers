
#------------------------------------------------
# Lift charts
#------------------------------------------------
library(ISLR)
library(tidyverse)
data("Default")
library(caret)
logit_fit3 <- glm(default ~ balance, 
                  family = binomial,
                  data = Default)
preds_DF <- data.frame(
  preds=predict(logit_fit3, 
                type = "response"),
  true = factor(Default$default, 
                levels = c("Yes","No"))
)
creditLift <- lift(true ~ preds, 
                   data = preds_DF)
xyplot(creditLift, 
       main = "X = balance")

library('caret')
logit_fit2 <- glm(default ~ student, 
                  family = binomial,
                  data = Default)
preds_DF <- data.frame(
  preds=predict(logit_fit2, 
                type = "response"),
  true = factor(Default$default, 
                levels = c("Yes","No"))
)
creditLift <- lift(true ~ preds, 
                   data = preds_DF)
xyplot(creditLift, 
       main = "X = student")

#------------------------------------------------
# Calibration charts
#------------------------------------------------
scores3DF <- data.frame(default = 
                          ifelse(Default$default == "Yes",1,0),
                        scores = 
                          predict(logit_fit3, type = "response"))
library(plyr)
calData <- ddply(scores3DF, .(cut(scores3DF$scores, 
                                  c(0,0.05,0.15,0.25,0.35,0.45,
                                    0.55,0.65,0.75,0.85,0.95,1))), 
                 colwise(mean))
calData$midpoint <- c(0.025,.1,.2,.3,.4,.5,.6,.7,.8,.9,.975)
colnames(calData) <- c("preds", "true", "midpoint")
calPlot <- ggplot(calData, aes(x = midpoint, y = true)) + 
  geom_point() + ylim(0,1) + 
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  xlab("Prediction midpoint") + ylab("Observed event percentage")
plot(calPlot)

#------------------------------------------------
## Downsampling and up-sampling
#------------------------------------------------
library('ROSE')
data_rose_down <- ROSE(default ~., data = Default, 
                       N = 666, p = 1/2)

table(data_rose_down$data$default)

data_rose_up <- ROSE(default ~., 
                     data = Default, 
                     N = 12000, 
                     p = 1/2)
table(data_rose_up$data$default)

# logit downsampled model
logit_down <- glm(default ~ balance,
                  data= data_rose_down$data,
                  family = "binomial")
summary(logit_down)

# logit up-sampled
logit_up <- glm(default ~ balance,
                data= data_rose_up$data,
                family = "binomial")
summary(logit_up)

# vanilla logit
logit <- glm(default ~ balance,
             data = Default,
             family = "binomial")

# generate scores and class predictions
scores_down = predict(logit_down,
                      type = "response")

scores_up = predict(logit_up,
                    type = "response")

scores_reg = predict(logit,
                     type = "response")

class_down = ifelse(scores_down > 0.5,1,0)
class_up = ifelse(scores_up > 0.5,1,0)
class_reg = ifelse(scores_reg > 0.5,1,0)
# 
# > diagnostics(table(data_rose_down$data$default,class_down))
# Accuracy:  0.872
# TP:  307
# TN:  274
# Sensitivity:  0.9
# Specificity:  0.843
# False Pos Rate:  0.157
# > diagnostics(table(data_rose_up$data$default,class_up))
# Accuracy:  0.869
# TP:  5282
# TN:  5149
# Sensitivity:  0.88
# Specificity:  0.859
# False Pos Rate:  0.141
# > diagnostics(table(Default$default,class_reg))


#------------------------------------------------
# Resampling Methods
#------------------------------------------------
# Auto dataset
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

library(caret)
RMSE(preds_DF$preds_LOOCV,preds_DF$true)
RMSE(preds_DF$preds_insample,preds_DF$true)
R2(preds_DF$preds_LOOCV,preds_DF$true)
R2(preds_DF$preds_insample,preds_DF$true)

#------------------------------------------------
### k-fold Cross validation
#------------------------------------------------
Auto_sub <- 
  mutate(Auto_sub,
  folds = createFolds(Auto_sub$mpg,
                      k = 10, list = FALSE)
)

### K-Fold Cross Validation
nfolds <- 10
preds_10FoldCV_DF <- data.frame(
  folds = Auto_sub$folds,
  preds_10FoldCV = rep(NA,nrow(Auto_sub))
)
for(i in 1:nfolds){
  mod <- lm(mpg ~ ., data = Auto_sub %>% filter(folds != i))
  preds <- predict(mod, newdata = filter(Auto_sub,folds == i))
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
# more complicated function for bootstrapping
#------------------------------------------------
library(rsample)
library(broom)
library(purrr)
library(boot)


boots <- bootstraps(Auto_sub, times = 100)
boots


fit_lm_on_boots <- function(split){
  lm(mpg ~ displacement - 1, 
     data = analysis(split))
}

boot_mods <- boots %>% 
  mutate(model = map(splits, fit_lm_on_boots),
         coef_info = map(model, tidy))

boot_coefs <- boot_mods %>% unnest(coef_info)

boot_coefs

ggplot(boot_coefs, aes(estimate)) + geom_histogram(bins = 20) 
