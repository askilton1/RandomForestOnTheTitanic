rm(list=ls())
library(dplyr)
train <- read.csv("train.csv")
test <- read.csv("test.csv")
source("cleanAndManipulate.R")
linearPredictors <- cleanAndManipulate(train,logit=TRUE)$predictors
Survived <- cleanAndManipulate(train,logit=TRUE)$Survived[[1]]

logit.mod <- glm(Survived~.,data=cbind(linearPredictors,Survived),family=binomial(link="logit"))
  library(boot)
  set.seed(1)
  cv.logit<-cv.glm(data=cbind(linearPredictors,Survived),logit.mod,cost,K=10)$delta[1];cv.logit

logit.ridge.mod <- glmnet(data.matrix(linearPredictors),Survived,alpha=0,lambda=grid,family="binomial")
  set.seed(1)
  cv.out <- cv.glmnet(data.matrix(linearPredictors),Survived,alpha=0,family="binomial")
  ridge.pred<-predict(logit.ridge.mod,s=cv.out$lambda.min,newx=data.matrix(linearPredictors))
  ridge.pred2 <- rep(0,length(ridge.pred));ridge.pred2[ridge.pred > .5] <- 1
  mean(ridge.pred2!=f2n(Survived))
  logit.ridge.coefs <- predict(cv.out,type="coefficients",s=cv.out$lambda.min)

logit.lasso.mod <- glmnet(data.matrix(linearPredictors),Survived,alpha=1,lambda=grid,family="binomial")
  set.seed(1)
  cv.out <- cv.glmnet(data.matrix(linearPredictors),Survived,alpha=1,family="binomial")
  lasso.pred<-predict(logit.lasso.mod,s=cv.out$lambda.min,newx=data.matrix(linearPredictors))
  lasso.pred2 <- rep(0,length(lasso.pred));lasso.pred2[lasso.pred > .5] <- 1
  mean(lasso.pred2!=f2n(Survived))
  logit.lasso.coefs <- predict(cv.out,type="coefficients",s=cv.out$lambda.min)
  
cbind(logit.ridge.coefs,logit.lasso.coefs)
mean(ridge.pred2==lasso.pred2)#ridge and lasso agree with each other 94.8% of the time
