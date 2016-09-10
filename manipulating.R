rm(list=ls())
library(dplyr)
train <- tbl_df(read.csv("train.csv")) 
test <- tbl_df(read.csv("test.csv")) 

train$title <- NA
for(string in c("master","mr","miss","mrs")) train$title[grepl(string,train$Name,ignore.case=T)] <- string
train$title <- as.factor(train$title)
train$title[is.na(train$title)] <- ifelse(train$Sex[is.na(train$title)]=="female","mrs","mr")

train$cabin_letter <- "U"
for(letter in c("A","B","C","D","E","F","T","G")) train$cabin_letter[grepl(letter,train$Cabin)] <- letter
train$cabin_letter <- as.factor(train$cabin_letter)
train <- cbind(train,predict(dummyVars(~cabin_letter, data = train), newdata = train))
attach(train);table(Survived,cabin_letter);detach(train)

require(caret)
train$Pclass <- as.factor(train$Pclass)
train <- cbind(train,predict(dummyVars(~Pclass, data = train), newdata = train))
train$title <- as.factor(train$title)
train <- cbind(train,predict(dummyVars(~title, data = train), newdata = train))

#Imputing Data
train$Embarked[train$Embarked==""] <- NA
library(caret)
set.seed(13343)
preObj <- preProcess(train[,-2],method="knnImpute")
Embarked <- predict(preObj,train[,-2])$Embarked

train.rf<-train %>%
  mutate(sex_age = ifelse(is.na(Age),as.character(Sex),ifelse(Age<=10 | (Age <= 15 & Parch>0),"child",as.character(Sex)))) %>%
  select(-title,-Pclass
         #,-familysize
         ) %>%
  select(Survived,Sex,sex_age,contains("title"),contains("Pclass"),contains("cabin_letter"),#contains("family"),
         Embarked,
         Fare,Parch) %>%
  mutate_all(as.factor) %>%
  na.omit() 
train.rf$Fare <- as.numeric(as.character(train.rf$Fare))

library(randomForest)
set.seed(1)
rf <- randomForest(Survived~.,data=train.rf,importance=T)
summary(glm(Survived~.,data=train.rf,family="binomial"))
rf$importance
rf


# submission$Survived <- predict(rf, extractFeatures(test))
