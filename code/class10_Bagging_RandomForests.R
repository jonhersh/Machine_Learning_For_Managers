#---------------------------------------------------------------
# Bagging
#---------------------------------------------------------------
rm(list = ls())
library(ElemStatLearn)
library('tidyverse')
library('partykit')
library('magrittr')
data(SAheart)

set.seed(111)
# generate testing and training sets
train_idx <- sample(1:nrow(SAheart), 
                    size = 0.75 * nrow(SAheart))

# convert outcome variable to factor 
# bc this is a classification problem)
SAheart %<>% mutate(chd = factor(chd,
                    levels = c("0","1"))) 

SA_train <- SAheart %>% slice(train_idx)
SA_test <- SAheart %>% slice(-train_idx)

# store rownames as columns
SA_train_preds <- SA_train %>% rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname))



# bagging - bootstrapp aggregation
B <- 100      # number of bootstrap samples
num_b <- 250  # sample size of each bootstrap
boot_mods <- list() # store our bagging models
for(i in 1:B){
  boot_idx <- sample(1:nrow(SA_train), 
                     size = num_b,
                     replace = FALSE)
  # fit a tree on each bootstrap sample
  boot_tree <- ctree(chd ~ ., 
                        data = SA_train %>% 
                       slice(boot_idx)) 
  # store bootstraped model
  boot_mods[[i]] <- boot_tree
  # generate predictions for that bootstrap model
  preds_boot <- data.frame(
    preds_boot = predict(boot_tree),
    rowname = boot_idx 
  )  
  
  # rename prediction to indicate which boot iteration it came from
  names(preds_boot)[1] <- paste("preds_boot",i,sep = "")
  
  # merge predictions to SA_train dataset
  SA_train_preds <- left_join(x = SA_train_preds, y = preds_boot,
                          by = "rowname")
}

## examine some of the individual models
plot(boot_mods[[1]])

plot(boot_mods[[10]])

plot(boot_mods[[20]])


library(magrittr)
# must convert factor into numeric, note that class "0" = 1, 
# and class "1" = 2, so we need to subtract 1 from every column
SA_train_preds %<>% mutate_if(is.factor, as.numeric) %>% 
  mutate_all(function(x){x - 1})

# calculate mean over all the bootstrap predictions
SA_train_preds %<>% mutate(preds_bag = 
                            select(., preds_boot1:preds_boot100) %>% 
                            rowMeans(na.rm = TRUE))

# congratulations! You have bagged your first model!
ggplot(SA_train_preds, aes(x = preds_bag)) + geom_histogram()


#---------------------------------------------------------------
# Random Forest
#---------------------------------------------------------------
library('randomForest')

rf_fit <- randomForest(chd ~ . ,
                       data = SA_train,
                       type = classification,
                       mtry = 3,
                       ntree = 1000,
                       importance = TRUE,
                       localImp = TRUE)

rf_fit

#---------------------------------------------------------------
# Explaining Random Forests
#---------------------------------------------------------------
# install.packages('randomForestExplainer')
library(randomForestExplainer)
# plot min
plot_min_depth_distribution(rf_fit)

plot_multi_way_importance(rf_fit)

plot_multi_way_importance(rf_fit, x_measure = "mse_increase",
                          y_measure = "node_purity_increase")

plot_predict_interaction(rf_fit, SA_train, "tobacco", "age")

# explanation file 
explain_forest(rf_fit, interactions = TRUE, data = SA_train)

#---------------------------------------------------------------
# Tuning Random Forests
#---------------------------------------------------------------
rf_mods <- list()
oob_err <- NULL
test_err <- NULL
for(mtry in 1:9){
  rf_fit <- randomForest(chd ~ ., 
                         data = SA_train,
                         mtry = mtry,
                         ntree = 500)
  oob_err[mtry] <- rf_fit$err.rate[500]

  cat(mtry," ")
}

results_DF <- data.frame(mtry = 1:9,
                 oob_err)
ggplot(results_DF, aes(x = mtry, y = oob_err)) + geom_point()
