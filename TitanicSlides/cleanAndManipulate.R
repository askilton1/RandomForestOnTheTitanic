cleanAndManipulate <- function(raw,linear=FALSE){
raw$familySize <- raw$Parch + raw$SibSp

#title
raw$title <- NA
for(string in c("master","mr","miss","mrs")) raw$title[grepl(string,raw$Name,ignore.case=T)] <- string
raw$title[is.na(raw$title)] <- ifelse(raw$Sex[is.na(raw$title)]=="female","mrs","mr")
require(caret)
raw$title <- as.factor(raw$title)
raw <- cbind(raw,predict(dummyVars(~title, data = raw), newdata = raw))


#Pclass
raw$Pclass <- as.factor(raw$Pclass);raw <- cbind(raw,predict(dummyVars(~Pclass, data = raw), newdata = raw))
#Embarked
raw <- raw[raw$Embarked!="",];raw$Embarked<-droplevels((raw$Embarked))
raw <- cbind(raw,predict(dummyVars(~Embarked, data = raw), newdata = raw))
#cabin
raw$cabin <- 0
raw$cabin[raw$Cabin==""] <- 1
#Ticket
raw$Ticket_numeric <- as.numeric(gsub("[^0-9]","",raw$Ticket))
raw$Ticket_group[raw$Ticket_numeric < 300000] <- "lowTicketNumber"
raw$Ticket_group[raw$Ticket_numeric >= 300000] <- "highTicketNumber"
raw <- cbind(raw,predict(dummyVars(~Ticket_group, data = raw), newdata = raw))


# library(caret)
# set.seed(13343)
# preObj <- preProcess(raw[,-2],method="knnImpute")
# Embarked <- predict(preObj,raw[,-2])$Embarked

raw$adultMale <- 0; raw$adultFemale <- 0; raw$child <- 0 
raw$adultMale[is.na(raw$Age) & as.character(raw$Sex) == "male"] <- 1
raw$adultFemale[is.na(raw$Age) & as.character(raw$Sex) == "female"] <- 1
raw$child[raw$Age<=10 | (raw$Age <= 15 & raw$Parch>0) | raw$SibSp > 1 | raw$title=="master"] <- 1

library(dplyr)
raw<-tbl_df(raw)
raw<-raw %>%
  dplyr::select(-title,-Pclass,-Embarked,-Ticket_numeric,-Sex,-title.master,-title.miss) %>%
  dplyr::select(Survived,child,contains("title"),contains("Pclass"),cabin,contains("Embarked"),contains("group"),Fare,Parch,familySize) %>%
  na.omit() %>%
  mutate_if(function(col) ifelse(is.factor(col),FALSE,max(col)==1 & min(col)==0),as.factor)

raw$Ticket_group <- as.factor(raw$Ticket_group)
if(linear==FALSE){
  clean <- list() 
  clean$predictors <- raw %>% dplyr::select(-Survived) 
  clean$Survived <- raw %>% dplyr::select(Survived) %>% mutate_all(as.factor)
} else if (linear==TRUE){
  raw <- raw %>%
    dplyr::select(-title.mr,-Pclass.3,-Embarked.S,-Ticket_grouphighTicketNumber)
  clean <- list() 
  clean$predictors <- raw %>% dplyr::select(-Survived) 
  clean$Survived <- raw %>% dplyr::select(Survived) %>% mutate_all(as.factor)
}
return(clean)
} 