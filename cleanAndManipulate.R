cleanAndManipulate <- function(raw){

raw$familySize <- raw$Parch + raw$SibSp
  
raw$title <- NA
for(string in c("master","mr","miss","mrs")) raw$title[grepl(string,raw$Name,ignore.case=T)] <- string
raw$title <- as.factor(raw$title)
raw$title[is.na(raw$title)] <- ifelse(raw$Sex[is.na(raw$title)]=="female","mrs","mr")
require(caret)
raw <- cbind(raw,predict(dummyVars(~title, data = raw), newdata = raw))
raw$Pclass <- as.factor(raw$Pclass);raw <- cbind(raw,predict(dummyVars(~Pclass, data = raw), newdata = raw))

raw$cabin_letter <- "U"
for(letter in c("A","B","C","D","E","F","T","G")) raw$cabin_letter[grepl(letter,raw$Cabin)] <- letter
raw$cabin_letter <- as.factor(raw$cabin_letter)
raw <- cbind(raw,predict(dummyVars(~cabin_letter, data = raw), newdata = raw))
#attach(raw);table(Survived,cabin_letter);detach(raw)

#Imputing Data
raw$Embarked[raw$Embarked==""] <- NA
library(caret)
set.seed(13343)
preObj <- preProcess(raw[,-2],method="knnImpute")
Embarked <- predict(preObj,raw[,-2])$Embarked

raw$adultMale <- 0; raw$adultFemale <- 0; raw$child <- 0 
raw$adultMale[is.na(raw$Age) & as.character(raw$Sex) == "male"] <- 1
raw$adultFemale[is.na(raw$Age) & as.character(raw$Sex) == "female"] <- 1
raw$child[raw$Age<=10 | (raw$Age <= 15 & raw$Parch>0) | raw$SibSp > 1 | raw$title=="master"] <- 1

ifelse(is.na(raw$Age),as.character(raw$Sex),
       ifelse(raw$Age<=10 | (raw$Age <= 15 & raw$Parch>0),"child",as.character(raw$Sex)))

library(dplyr)
raw<-tbl_df(raw)
raw<-raw %>%
  dplyr::select(-title,-Pclass,-cabin_letter.G,-cabin_letter.A,-cabin_letter.T,-cabin_letter.C,-sex_age) %>%
  dplyr::select(-Sex) %>%
  #dplyr::select(-Cabin,-Ticket,-Name,cabin_letter.B) %>%
  dplyr::select(Survived,
         #Sex,
         contains("sex_age"),contains("title"),contains("Pclass"),contains("cabin_letter"),Embarked,Fare,Parch,familySize,
         PassengerId) %>%
  #mutate_at(as.factor) %>%
  na.omit %>%
  mutate_if(function(col) ifelse(is.factor(col),FALSE,max(col)==1 & min(col)==0),as.factor)
raw$Fare <- as.numeric(as.character(raw$Fare)) 

clean <- list() 
clean$predictors <- raw %>% dplyr::select(-Survived) 
clean$Survived <- raw %>% dplyr::select(Survived) %>% mutate_all(as.factor) 

return(clean)} 