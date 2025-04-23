
df <- read.csv("~/Documents/3.Winter2023/600-Data Mining/Week 10 Final/heart_failure_clinical_records_dataset.csv")

library(tidyverse)
library(skimr)      # skimming data frames
library(patchwork)  # combine separate ggplots into the same graphic
library(corrplot)
library(caret)      # confusionMatrix()
library(rpart)      # Recursive Partitioning and Regression Trees
library(rpart.plot)

############# Data visualization

f_features = c("anaemia", "diabetes", "high_blood_pressure", "sex", "smoking", "DEATH_EVENT")

df_n <- df
df <- df %>%
  mutate_at(f_features, as.factor)



#### sub-section:  Multiple Linear Regression

# Split the data into training and testing sets
set.seed(123)  # for reproducibility
train_index <- sample(nrow(df_n), 0.7 * nrow(df_n))
train_data <- df_n[train_index, ]
test_data <- df_n[-train_index, ]


 filter(df, !complete.cases(df) )
#### sub-section:  Multiple Linear Regression

## obtain a linear regression model for predicting the DEATH_EVENT on heart failure.
lm.a1 <- lm(DEATH_EVENT ~ ., data = train_data)

## obtain more details on the linear model
summary(lm.a1)

## 
anova(lm.a1)

## Find variable that least contributes to the reduction of the fitting error of the model from above result. 
##And remove it from the model using below operation
#These results indicate that the variable smoking is the variable that least contributes to the reduction of the fitting error of the model. Let us remove it from the model:

lm2.a1 <- update(lm.a1, . ~ . - smoking)

  
## The summary information for this new model is given below:
summary(lm2.a1)

#We can carry out a more formal comparison between the two models by again using the anova() function, but this time with both models
anova(lm.a1,lm2.a1)

## creates a linear model that results from applying the backward elimination method to the initial model we have obtained (lm.a1)
final.lm <- step(lm.a1)

## btain the information on the final model by
summary(final.lm)





#### sub-section:  Regression Trees

##
library(rpart)

rt.a1 <- rpart(DEATH_EVENT ~ ., data = train_data)

##
rt.a1

##
library(rpart.plot)

prp(rt.a1,extra=101,box.col="orange",split.box.col="grey")


##
printcp(rt.a1)

##
rt2.a1 <- prune(rt.a1, cp = 0.07)
rt2.a1

##
rt.a1 <- rpartXse(DEATH_EVENT ~ ., data = train_data)
rt.a1

lm.a1 <- lm(DEATH_EVENT ~ ., data = train_data)

final.lm <- step(lm.a1)

rt.a1 <- rpart(DEATH_EVENT ~ ., data = train_data)


####### Section:  Model Evaluation and Selection


##
lm.predictions.a1 <- predict(final.lm, train_data)
rt.predictions.a1 <- predict(rt.a1, train_data)

##
(mae.a1.lm <- mean(abs(lm.predictions.a1 - train_data[["DEATH_EVENT"]])))
(mae.a1.rt <- mean(abs(rt.predictions.a1 - train_data[["DEATH_EVENT"]])))

##
(mse.a1.lm <- mean((lm.predictions.a1 - train_data[["DEATH_EVENT"]])^2))
(mse.a1.rt <- mean((rt.predictions.a1 - train_data[["DEATH_EVENT"]])^2))

##
(nmse.a1.lm <- mean((lm.predictions.a1-train_data[["DEATH_EVENT"]])^2)/
    mean((mean(train_data[["DEATH_EVENT"]])-train_data[["DEATH_EVENT"]])^2))
(nmse.a1.rt <- mean((rt.predictions.a1-train_data[["DEATH_EVENT"]])^2)/
    mean((mean(train_data[["DEATH_EVENT"]])-train_data[["DEATH_EVENT"]])^2))

##
library(ggplot2)

dg <- data.frame(lm.a1=lm.predictions.a1,
                 rt.a1=rt.predictions.a1,
                 true.a1=train_data[["DEATH_EVENT"]])

ggplot(dg,aes(x=lm.a1,y=true.a1)) +
  geom_point() + geom_abline(slope=1,intercept=0,color="red") +
  ggtitle("Linear Model")

ggplot(dg,aes(x=rt.a1,y=true.a1)) +
  geom_point() + geom_abline(slope=1,intercept=0,color="red") +
  ggtitle("Regression Tree")


library(grid)

dg <- data.frame(lm.a1=lm.predictions.a1,rt.a1=rt.predictions.a1,true.a1=train_data[["DEATH_EVENT"]])

g1 <- ggplot(dg,aes(x=lm.a1,y=true.a1)) +
  geom_point() + geom_abline(slope=1,intercept=0,color="red") +
  ggtitle("Linear Model")

g2 <- ggplot(dg,aes(x=rt.a1,y=true.a1)) +
  geom_point() + geom_abline(slope=1,intercept=0,color="red") +
  ggtitle("Regression Tree")

grid.newpage()

pushViewport(viewport(layout=grid.layout(1,2)))
print(g1,vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(g2,vp=viewport(layout.pos.row=1,layout.pos.col=2))



##
sensible.lm.predictions.a1 <- ifelse(lm.predictions.a1 < 0, 0, lm.predictions.a1)
(mae.a1.lm <- mean(abs(lm.predictions.a1 - train_data[["DEATH_EVENT"]])))
(smae.a1.lm <- mean(abs(sensible.lm.predictions.a1 - train_data[["DEATH_EVENT"]])))

##
library(performanceEstimation)

res <- performanceEstimation(
  PredTask(DEATH_EVENT ~ ., train_data, "DEATH_EVENT"),
  c(Workflow(learner="lm",pre="knnImp",post="onlyPos"),
    workflowVariants(learner="rpartXse",learner.pars=list(se=c(0,0.5,1)))),
  EstimationTask(metrics="nmse",method=CV(nReps=5,nFolds=10))
)

##
summary(res) 

##
plot(res)

##
getWorkflow("rpartXse.v1", res)

##
topPerformers(res)

##
library(randomForest)

res.all <- performanceEstimation(
  PredTask(DEATH_EVENT ~ ., train_data, "DEATH_EVENT"),
  c(Workflow(learner="lm", pre="knnImp",post="onlyPos"),
    workflowVariants(learner="rpartXse",
                     learner.pars=list(se=c(0,0.5,1))),
    workflowVariants(learner="randomForest", pre="knnImp",
                     learner.pars=list(ntree=c(200,500,700)))),
  EstimationTask(metrics="nmse",method=CV(nReps=5,nFolds=10)))

##
rankWorkflows(res.all)



library(ggplot2)
library(DMwR2)
library(performanceEstimation)
library(randomForest)


################. Predictions

wfs <- sapply(taskNames(res.all),
              function(t) topPerformer(res.all,metric="nmse",task=t))

wfs[["DEATH_EVENT"]]



##

pts <- array(dim = c(90,1,2),
             dimnames = list(1:90, "DEATH_EVENT", c("trues","preds")))

res <- runWorkflow(wfs[["DEATH_EVENT"]],
                   as.formula(paste("DEATH_EVENT","~.")),
                   train_data,
                   test_data)
pts[,1,"trues"] <- res$trues
pts[,1,"preds"] <- res$preds

pts[1:5,c("DEATH_EVENT"),]



avg.preds <- apply(train_data[13], 2, mean)
avg.preds


apply((pts[,,"trues"] - pts[,,"preds"])^2 ,sum) /
  apply( (scale(pts[,,"trues"], avg.preds, FALSE))^2, 2, sum)

apply((pts[,,"trues"] - pts[,,"preds"])^2,2 ,sum) 
apply((pts[,,"trues"] - pts[,,"preds"])^2, 2, sum)


pts[,,"trues"]





test.set <- test_data[, c(1:12, 13)] 
train.set <- train_data[, c(1:12, 13)]

res <- runWorkflow(wfs[[1]], as.formula("DEATH_EVENT ~ ."), 
                   train.set, test.set)
trues <- res$trues
preds <- res$preds

avg.pred <- mean(train.set[,13])
avg.pred
sum((trues - preds)^2) / sum((scale(trues, avg.pred, FALSE))^2)



