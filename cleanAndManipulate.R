cleanAndManipulate <- function(raw){
  
raw$title <- NA
for(string in c("master","mr","miss","mrs")) raw$title[grepl(string,raw$Name,ignore.case=T)] <- string
raw$title <- as.factor(raw$title)
raw$title[is.na(raw$title)] <- ifelse(raw$Sex[is.na(raw$title)]=="female","mrs","mr")

raw$cabin_letter <- "U"
for(letter in c("A","B","C","D","E","F","T","G")) raw$cabin_letter[grepl(letter,raw$Cabin)] <- letter
raw$cabin_letter <- as.factor(raw$cabin_letter)
raw <- cbind(raw,predict(dummyVars(~cabin_letter, data = raw), newdata = raw))
#attach(raw);table(Survived,cabin_letter);detach(raw)

require(caret)
raw$Pclass <- as.factor(raw$Pclass)
raw <- cbind(raw,predict(dummyVars(~Pclass, data = raw), newdata = raw))
raw$title <- as.factor(raw$title)
raw <- cbind(raw,predict(dummyVars(~title, data = raw), newdata = raw))

#Imputing Data
raw$Embarked[raw$Embarked==""] <- NA
library(caret)
set.seed(13343)
preObj <- preProcess(raw[,-2],method="knnImpute")
Embarked <- predict(preObj,raw[,-2])$Embarked

library(dplyr)
raw<-raw %>%
  mutate(sex_age = ifelse(is.na(Age),as.character(Sex),ifelse(Age<=10 | (Age <= 15 & Parch>0),"child",as.character(Sex)))) %>%
  select(-title,-Pclass) %>%
  select(Survived,Sex,sex_age,contains("title"),contains("Pclass"),contains("cabin_letter"),Embarked,Fare,Parch) %>%
  mutate_all(as.factor) %>%
  na.omit() 
raw$Fare <- as.numeric(as.character(raw$Fare))

clean <- list()
clean$predictors <- raw %>% select(-Survived)
clean$Survived <- raw %>% select(Survived)

return(clean)}