rm(list=ls())
source("cleanAndManipulate.R")
source("ROC.curve.R")
source("tpr.R")
library(MASS)
library(ipred)
library(class)
library(randomForest)
library(glmnet)
library(ROCR)
library(boot)
library(glmnet)
f2n <- function(fac) as.numeric(as.character(fac))
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
grid <- 10^seq(10,-2, length=100)#possible values for lambda
linearPredictors<-cleanAndManipulate(train,linear=TRUE)$predictors
logit.mod <- glm(Survived~.,data=cbind(linearPredictors,Survived),family=binomial(link="logit"))
set.seed(1)
cv.logit<-cv.glm(data=cbind(linearPredictors,Survived),logit.mod,cost,K=10)$delta[1]
#results[4,2]<-truePositiveRate(predict(logit.mod),response.train)$tpr
#cm.ls$logit.mod <- truePositiveRate(predict(logit.mod),response.train)$cm
#results[4,3]<-ROC.curve(fitted(logit.mod),response.train)
logit.ridge.mod <- glmnet(data.matrix(linearPredictors),Survived,alpha=0,lambda=grid,family="binomial")
set.seed(1)
cv.out <- cv.glmnet(data.matrix(linearPredictors),Survived,alpha=0,family="binomial")
ridge.pred<-predict(logit.ridge.mod,s=cv.out$lambda.min,newx=data.matrix(linearPredictors))
ridge.pred2 <- rep(0,length(ridge.pred))
ridge.pred2[ridge.pred > .5] <- 1
mean(ridge.pred2==f2n(Survived))
logit.lasso.mod <- glmnet(data.matrix(linearPredictors),Survived,alpha=1,lambda=grid,family="binomial")
set.seed(1)
cv.out <- cv.glmnet(data.matrix(linearPredictors),Survived,alpha=1,family="binomial")
ridge.pred<-predict(logit.lasso.mod,s=cv.out$lambda.min,newx=data.matrix(linearPredictors))
ridge.pred2 <- rep(0,length(ridge.pred))
ridge.pred2[ridge.pred > .5] <- 1
mean(ridge.pred2==f2n(Survived))

results[6,1]<-cost(ridge.pred,f2n(response.train))
results[6,2]<-truePositiveRate(ridge.pred,f2n(response.train))$tpr
cm.ls$logit.lasso.mod <- truePositiveRate(ridge.pred,f2n(response.train))$cm
results[6,3]<-ROC.curve(ridge.pred,f2n(response.train))
rm(logit.lasso.mod)