cleanAndManipulate <- function(raw, rm.vars=TRUE,test=FALSE){

  raw$Cabin[raw$Cabin==""] <- NA
  raw$familySize <- raw$Parch + raw$SibSp

  #title
  raw$title <- NA
  for(string in c("master","mr","miss","mrs")) raw$title[grepl(string,raw$Name,ignore.case=T)] <- string
  raw$title[is.na(raw$title)] <- ifelse(raw$Sex[is.na(raw$title)]=="female","mrs","mr")
  raw$title <- as.factor(raw$title)
  
  require(caret)
  raw <- cbind(raw,predict(dummyVars(~title, data = raw), newdata = raw))
  
  raw$child <- 0;raw$child[raw$Age<=10 | (raw$Age <= 15 & raw$Parch>0) | raw$SibSp > 1 | raw$title=="master"] <- 1
  
  raw$ageMissing <- ifelse(is.na(raw$Age),1,0)
  
  #Pclass
  raw$Pclass <- as.factor(raw$Pclass);raw <- cbind(raw,predict(dummyVars(~Pclass, data = raw), newdata = raw))
  
  #Embarked
  raw <- raw[raw$Embarked!="",];raw$Embarked<-droplevels((raw$Embarked))
  raw <- cbind(raw,predict(dummyVars(~Embarked, data = raw), newdata = raw))
  
  #cabin
  raw$cabin <- ifelse(is.na(raw$Cabin),1,0)
 
  #Ticket
  raw$Ticket_numeric <- as.numeric(gsub("[^0-9]","",raw$Ticket))
  raw$Ticket_group <- ifelse(raw$Ticket_numeric < 300000,"lowTicketNumber","highTicketNumber")
  raw$Ticket_group <- as.factor(raw$Ticket_group)
  
  raw <- cbind(raw,predict(dummyVars(~Ticket_group, data = raw), newdata = raw))
  if(test==FALSE){
    library(dplyr)
    raw<-tbl_df(raw)
    raw<-raw %>%
      dplyr::select(-Ticket_numeric,-Sex,-title.master,-title.miss,-Cabin,-Pclass,-title) %>%
      dplyr::select(Survived,child,contains("title"),contains("Pclass"),cabin,contains("group"),Fare,Parch,familySize,ageMissing) %>%
      na.omit() %>%
      mutate_if(function(col) ifelse(is.factor(col),FALSE,max(col)==1 & min(col)==0),as.factor)
  
    if(rm.vars==TRUE){ 
      clean <- list() 
      clean$predictors <- dplyr::select(raw,-Survived) 
      clean$Survived <- raw %>% dplyr::select(Survived) %>% mutate_all(as.factor)
      return(clean)
    } else {
    return(raw)}
  } else {
    library(dplyr)
    raw<-tbl_df(raw)
    raw<-raw %>%
      dplyr::select(-Ticket_numeric,-Sex,-title.master,-title.miss,-Cabin,-Pclass,-title) %>%
      dplyr::select(child,contains("title"),contains("Pclass"),cabin,contains("group"),Fare,Parch,familySize,ageMissing) %>%
      na.omit() %>%
      mutate_if(function(col) ifelse(is.factor(col),FALSE,max(col)==1 & min(col)==0),as.factor)
    
    if(rm.vars==TRUE){ 
      return(raw)
    } else {
      return(raw)}
  }
}