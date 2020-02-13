# Lecture 5
#-------------------------------------------------------------------------------
## classification 
#-------------------------------------------------------------------------------
library(ISLR)
data(Default)
options(scipen = 3)

library(magrittr)
library(tidyverse)
library(ggExtra)

# create a binary version of default
Default %<>% mutate(default_binary = 
                      ifelse(default == "Yes", 1,0)) 

summary(Default)

# estimate an OLS model using the 0,1 
# variable as our dependent variable
mod1 <- lm(default_binary ~ balance, 
           data = Default)
summary(mod1)

preds_DF <- data.frame(
  preds = predict(mod1),
  Default
)

# what kind of predictions do we get for this model?
head(preds_DF)

p <- ggplot(preds_DF, aes(x = balance, 
                          y = default_binary)) + 
  geom_point(alpha = 1/20) +
  geom_abline(intercept = mod1$coefficients[1], 
              slope = mod1$coefficients[2],
              color = "red", linetype = "dashed")

plot(p)
p <- ggMarginal(p, type = "histogram", fill="purple", size=6)
plot(p)

##
library(ISLR)
data(Default)
options(scipen=9)
library('tidyverse')

logitMod1 <- glm(factor(default) ~ balance, 
               family = binomial,
               data = Default)

summary(logitMod1)

round(logitMod1$coefficients,4)

top <- exp(logitMod1$coefficients[1] + 
             logitMod1$coefficients[2] * 2000)
p_hat_1000 <- top / (1 + top)

p_hat_1000

scores <- predict(logitMod1,
                  type = "response")
head(scores)

preds_DF <- data.frame(
  scores_mod1 = scores,
  Default
)


ggplot(preds_DF, aes(x = default, y = scores)) + 
  geom_boxplot(alpha = 1/10) 

ggplot(preds_DF, aes(x = scores_mod1)) + 
  geom_histogram(data = preds_DF %>% 
                   filter(default == "No"), 
                 fill = "red", alpha = 0.2) +
  geom_histogram(data = preds_DF %>% 
                   filter(default == "Yes"),
                 fill = "blue", alpha = 1) + 
  labs(x = "Estimated Probabilities (scores)",
       y = "count")

ggplot(preds_DF, aes(x = scores_mod1)) + 
  geom_density(data = preds_DF %>% 
                 filter(default == "No"), 
               fill = "red", alpha = 0.2) +
  geom_density(data = preds_DF %>% 
                 filter(default == "Yes"),
               fill = "blue", alpha = 1) + 
  labs(x = "Estimated Probabilities (scores)",
       y = "count")

### adding more variables
logitMod2 <- glm(factor(default) ~ balance  + student + income, 
               family = binomial,
               data = Default)

summary(logitMod2)

preds_DF <- data.frame(
  scores_mod1 = predict(logitMod1, type = "response"),
  scores_mod2 = predict(logitMod2, type = "response"),
  Default
)



## Confusion Matrices
library('caret')
?confusionMatrix
confusionMatrix(factor(ifelse(preds_DF$scores_mod1 > 0.5,"Yes","No"), 
                       levels = c("Yes", "No")),
                factor(preds_DF$default, 
                       levels = c("Yes","No"))
)

### ROC Curve
TrainDF <- data.frame(default = c(Default$default, Default$default),
                      scores = c(preds_DF$scores_mod1,
                                     preds_DF$scores_mod2),
                      models = c(rep("X = Student",length(preds_DF$scores_mod1)),
                                 rep("X = Student + Balance + Income",
                                     length(preds_DF$scores_mod2))))


library(ggplot2)
library('plotROC')
# install.packages("devtools")
# library(devtools)
TrainROC <- ggplot(TrainDF, aes(m = scores, d = default, color = models)) + 
  geom_roc(show.legend = TRUE, labelsize = 3.5, cutoffs.at = c(.99,.9,.7,.5,.3,.1,0))
TrainROC <- TrainROC + style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  theme(legend.justification = c(1, 0), 
        legend.position = c(1, 0),
        legend.box.margin=margin(c(50,50,50,50)))
plot(TrainROC)


# calibration plot
scores3DF <- data.frame(default = ifelse(Default$default == "Yes",1,0),
                        scores = preds_DF$scores_mod2)
library(plyr)
calData <- ddply(scores3DF, .(cut(scores3DF$scores, c(0,0.05,0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85,0.95,1))), colwise(mean))
calData$midpoint <- c(0.025,.1,.2,.3,.4,.5,.6,.7,.8,.9,.975)
colnames(calData) <- c("preds", "true", "midpoint")
calPlot <- ggplot(calData, aes(x = midpoint, y = true)) + geom_point() + ylim(0,1) + 
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  xlab("Prediction midpoint") + ylab("Observed event percentage")

pdf("CalPlot.pdf", width=7, height=5)
plot(calPlot)
dev.off()


