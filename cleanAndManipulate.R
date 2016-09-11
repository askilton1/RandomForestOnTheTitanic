cleanAndManipulate <- function(raw,linear=FALSE){
raw$familySize <- raw$Parch + raw$SibSp
  
raw$title <- NA
for(string in c("master","mr","miss","mrs")) raw$title[grepl(string,raw$Name,ignore.case=T)] <- string
raw$title <- as.factor(raw$title)
raw$title[is.na(raw$title)] <- ifelse(raw$Sex[is.na(raw$title)]=="female","mrs","mr")
require(caret)
raw <- cbind(raw,predict(dummyVars(~title, data = raw), newdata = raw))
raw$Pclass <- as.factor(raw$Pclass);raw <- cbind(raw,predict(dummyVars(~Pclass, data = raw), newdata = raw))
raw <- cbind(raw,predict(dummyVars(~Embarked, data = raw), newdata = raw))

raw$cabin <- 0
raw$cabin[raw$Cabin==""] <- 1
#for(letter in c("A","B","C","D","E","F","T","G")) raw$cabin[grepl(letter,raw$Cabin)] <- 1
# raw$cabin_letter <- as.factor(raw$cabin_letter)
# raw <- cbind(raw,predict(dummyVars(~cabin_letter, data = raw), newdata = raw))
# attach(raw);table(Survived,cabin);detach(raw)

raw$Ticket_numeric <- as.numeric(gsub("[^0-9]","",train$Ticket))
raw$Ticket_numeric[raw$Ticket_numeric==3] <- NA

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

library(dplyr)
raw<-tbl_df(raw)
raw<-raw %>%
  dplyr::select(-title,-Pclass,-Embarked,-Ticket_numeric) %>%
  dplyr::select(-Sex) %>%
  #dplyr::select(-Cabin,-Ticket,-Name,cabin_letter.B) %>%
  dplyr::select(Survived,
                #Sex,
                contains("title"),contains("Pclass"),cabin,contains("Embarked"),Fare,Parch,familySize,
                PassengerId) %>%
  #mutate_at(as.factor) %>%
  na.omit %>%
  mutate_if(function(col) ifelse(is.factor(col),FALSE,max(col)==1 & min(col)==0),as.factor)
raw$Fare <- as.numeric(as.character(raw$Fare)) 

if(linear==FALSE){
  clean <- list() 
  clean$predictors <- raw %>% dplyr::select(-Survived) 
  clean$Survived <- raw %>% dplyr::select(Survived) %>% mutate_all(as.factor)
} else if (linear==TRUE){
  raw <- raw %>%
    dplyr::select(-title.mr,-Pclass.3,-Embarked.S)
  clean <- list() 
  clean$predictors <- raw %>% dplyr::select(-Survived) 
  clean$Survived <- raw %>% dplyr::select(Survived) %>% mutate_all(as.factor)
}
return(clean)
} 